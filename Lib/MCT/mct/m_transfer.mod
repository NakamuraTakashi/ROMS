V24 m_transfer
14 m_Transfer.F90 S582 0
01/06/2014  21:00:29
use m_router private
use m_attrvect private
use m_mctworld private
use m_router private
use m_attrvect private
use m_mctworld private
enduse
D 210 18 162
D 231 24 1089 624 1088 7
D 460 24 1703 1184 1702 7
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 m_transfer
S 584 23 0 0 0 6 973 582 4681 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 mctworld
S 585 23 0 0 0 8 990 582 4690 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 thismctworld
S 587 23 0 0 0 8 1088 582 4714 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 attrvect
S 588 19 0 0 0 6 1 582 4723 14 0 0 0 0 0 0 0 0 0 0 0 0 0 207 1 0 0 0 0 0 582 0 0 0 0 niattr
O 588 1 1202
S 589 19 0 0 0 6 1 582 4730 14 0 0 0 0 0 0 0 0 0 0 0 0 0 205 1 0 0 0 0 0 582 0 0 0 0 nrattr
O 589 1 1206
S 590 19 0 0 0 8 1 582 4737 14 0 0 0 0 0 0 0 0 0 0 0 0 0 203 1 0 0 0 0 0 582 0 0 0 0 permute
O 590 1 1359
S 591 19 0 0 0 8 1 582 4745 14 0 0 0 0 0 0 0 0 0 0 0 0 0 201 1 0 0 0 0 0 582 0 0 0 0 unpermute
O 591 1 1369
S 592 19 0 0 0 8 1 582 4755 10 0 0 0 0 0 0 0 0 0 0 0 0 0 199 3 0 0 0 0 0 582 0 0 0 0 attrvect_init
O 592 3 1186 1180 1175
S 594 19 0 0 0 8 1 582 4774 10 0 0 0 0 0 0 0 0 0 0 0 0 0 195 1 0 0 0 0 0 582 0 0 0 0 attrvect_copy
O 594 1 1340
S 596 19 0 0 0 8 1 582 4793 10 0 0 0 0 0 0 0 0 0 0 0 0 0 193 1 0 0 0 0 0 582 0 0 0 0 attrvect_clean
O 596 1 1190
S 598 19 0 0 0 6 1 582 4814 14 0 0 0 0 0 0 0 0 0 0 0 0 0 191 1 0 0 0 0 0 582 0 0 0 0 lsize
O 598 1 1193
S 600 23 0 0 0 8 1702 582 4829 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 router
S 970 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 973 25 2 m_mctworld mctworld
R 990 6 19 m_mctworld thismctworld
R 1088 25 5 m_attrvect attrvect
R 1089 5 6 m_attrvect ilist attrvect
R 1090 5 7 m_attrvect rlist attrvect
R 1093 5 10 m_attrvect iattr attrvect
R 1094 5 11 m_attrvect iattr$sd attrvect
R 1095 5 12 m_attrvect iattr$p attrvect
R 1096 5 13 m_attrvect iattr$o attrvect
R 1100 5 17 m_attrvect rattr attrvect
R 1101 5 18 m_attrvect rattr$sd attrvect
R 1102 5 19 m_attrvect rattr$p attrvect
R 1103 5 20 m_attrvect rattr$o attrvect
R 1175 14 92 m_attrvect init_
R 1180 14 97 m_attrvect initv_
R 1186 14 103 m_attrvect initl_
R 1190 14 107 m_attrvect clean_
R 1193 14 110 m_attrvect lsize_
R 1202 14 119 m_attrvect niattr_
R 1206 14 123 m_attrvect nrattr_
R 1340 14 257 m_attrvect copy_
R 1359 14 276 m_attrvect permute_
R 1369 14 286 m_attrvect unpermute_
R 1702 25 16 m_router router
R 1703 5 17 m_router comp1id router
R 1704 5 18 m_router comp2id router
R 1705 5 19 m_router nprocs router
R 1706 5 20 m_router maxsize router
R 1707 5 21 m_router lavsize router
R 1708 5 22 m_router numiatt router
R 1709 5 23 m_router numratt router
R 1711 5 25 m_router pe_list router
R 1712 5 26 m_router pe_list$sd router
R 1713 5 27 m_router pe_list$p router
R 1714 5 28 m_router pe_list$o router
R 1717 5 31 m_router num_segs router
R 1718 5 32 m_router num_segs$sd router
R 1719 5 33 m_router num_segs$p router
R 1720 5 34 m_router num_segs$o router
R 1723 5 37 m_router locsize router
R 1724 5 38 m_router locsize$sd router
R 1725 5 39 m_router locsize$p router
R 1726 5 40 m_router locsize$o router
R 1729 5 43 m_router permarr router
R 1730 5 44 m_router permarr$sd router
R 1731 5 45 m_router permarr$p router
R 1732 5 46 m_router permarr$o router
R 1736 5 50 m_router seg_starts router
R 1737 5 51 m_router seg_starts$sd router
R 1738 5 52 m_router seg_starts$p router
R 1739 5 53 m_router seg_starts$o router
R 1743 5 57 m_router seg_lengths router
R 1744 5 58 m_router seg_lengths$sd router
R 1745 5 59 m_router seg_lengths$p router
R 1746 5 60 m_router seg_lengths$o router
R 1749 5 63 m_router rp1 router
R 1750 5 64 m_router rp1$sd router
R 1751 5 65 m_router rp1$p router
R 1752 5 66 m_router rp1$o router
R 1755 5 69 m_router ip1 router
R 1756 5 70 m_router ip1$sd router
R 1757 5 71 m_router ip1$p router
R 1758 5 72 m_router ip1$o router
R 1761 5 75 m_router ireqs router
R 1762 5 76 m_router ireqs$sd router
R 1763 5 77 m_router ireqs$p router
R 1764 5 78 m_router ireqs$o router
R 1766 5 80 m_router rreqs router
R 1768 5 82 m_router rreqs$sd router
R 1769 5 83 m_router rreqs$p router
R 1770 5 84 m_router rreqs$o router
R 1774 5 88 m_router istatus router
R 1775 5 89 m_router istatus$sd router
R 1776 5 90 m_router istatus$p router
R 1777 5 91 m_router istatus$o router
R 1779 5 93 m_router rstatus router
R 1782 5 96 m_router rstatus$sd router
R 1783 5 97 m_router rstatus$p router
R 1784 5 98 m_router rstatus$o router
S 1975 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 600 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 2548 19 0 0 0 6 1 582 14666 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 348 1 0 0 0 0 0 582 0 0 0 0 isend
O 2548 1 2554
S 2549 19 0 0 0 8 1 582 5626 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 350 1 0 0 0 0 0 582 0 0 0 0 send
O 2549 1 2555
S 2550 19 0 0 0 8 1 582 14672 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 352 1 0 0 0 0 0 582 0 0 0 0 waitsend
O 2550 1 2556
S 2551 19 0 0 0 6 1 582 14681 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 354 1 0 0 0 0 0 582 0 0 0 0 irecv
O 2551 1 2557
S 2552 19 0 0 0 8 1 582 5631 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 356 1 0 0 0 0 0 582 0 0 0 0 recv
O 2552 1 2558
S 2553 19 0 0 0 8 1 582 14687 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 358 1 0 0 0 0 0 582 0 0 0 0 waitrecv
O 2553 1 2559
S 2554 27 0 0 0 6 2563 582 14696 10010 0 0 0 359 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 isend_
Q 2554 2548 0
S 2555 27 0 0 0 8 2571 582 5860 10010 0 0 0 361 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 send_
Q 2555 2549 0
S 2556 27 0 0 0 8 2568 582 14703 10010 0 0 0 360 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 waitsend_
Q 2556 2550 0
S 2557 27 0 0 0 6 2576 582 14713 10010 0 0 0 362 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 irecv_
Q 2557 2551 0
S 2558 27 0 0 0 8 2587 582 5866 10010 0 0 0 364 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 recv_
Q 2558 2552 0
S 2559 27 0 0 0 8 2582 582 14720 10010 0 0 0 363 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 waitrecv_
Q 2559 2553 0
S 2560 16 0 0 0 6 1 582 14730 14 400000 0 0 0 0 600 1013 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 defaulttag
S 2561 3 0 0 0 210 0 1 0 0 0 0 0 0 0 14741 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 15 4d 43 54 3a 3a 6d 5f 54 72 61 6e 73 66 65 72
S 2562 16 0 0 0 210 1 582 5137 14 440000 0 0 0 0 2561 1103 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 2563 23 5 0 0 0 2567 582 14696 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 isend_
S 2564 1 3 1 0 231 1 2563 7710 14 3008 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 avin
S 2565 1 3 3 0 460 1 2563 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2566 1 3 1 0 6 1 2563 14757 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2567 14 5 0 0 0 1 2563 14696 10 400000 0 0 0 584 3 0 0 0 0 0 0 0 0 0 0 0 0 103 0 582 0 0 0 0 isend_
F 2567 3 2564 2565 2566
S 2568 23 5 0 0 0 2570 582 14703 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 waitsend_
S 2569 1 3 3 0 460 1 2568 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2570 14 5 0 0 0 1 2568 14703 10 400000 0 0 0 588 1 0 0 0 0 0 0 0 0 0 0 0 0 287 0 582 0 0 0 0 waitsend_
F 2570 1 2569
S 2571 23 5 0 0 0 2575 582 5860 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 send_
S 2572 1 3 1 0 231 1 2571 7553 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2573 1 3 3 0 460 1 2571 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2574 1 3 1 0 6 1 2571 14757 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2575 14 5 0 0 0 1 2571 5860 10 400000 0 0 0 590 3 0 0 0 0 0 0 0 0 0 0 0 0 361 0 582 0 0 0 0 send_
F 2575 3 2572 2573 2574
S 2576 23 5 0 0 0 2581 582 14713 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 irecv_
S 2577 1 3 3 0 231 1 2576 7553 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2578 1 3 3 0 460 1 2576 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2579 1 3 1 0 6 1 2576 14757 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2580 1 3 1 0 16 1 2576 2783 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2581 14 5 0 0 0 1 2576 14713 10 400000 0 0 0 594 4 0 0 0 0 0 0 0 0 0 0 0 0 419 0 582 0 0 0 0 irecv_
F 2581 4 2577 2578 2579 2580
S 2582 23 5 0 0 0 2586 582 14720 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 waitrecv_
S 2583 1 3 3 0 231 1 2582 7553 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2584 1 3 3 0 460 1 2582 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2585 1 3 1 0 16 1 2582 2783 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2586 14 5 0 0 0 1 2582 14720 10 400000 0 0 0 599 3 0 0 0 0 0 0 0 0 0 0 0 0 583 0 582 0 0 0 0 waitrecv_
F 2586 3 2583 2584 2585
S 2587 23 5 0 0 0 2592 582 5866 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 recv_
S 2588 1 3 3 0 231 1 2587 7553 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2589 1 3 3 0 460 1 2587 9586 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2590 1 3 1 0 6 1 2587 14757 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2591 1 3 1 0 16 1 2587 2783 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2592 14 5 0 0 0 1 2587 5866 10 400000 0 0 0 603 4 0 0 0 0 0 0 0 0 0 0 0 0 790 0 582 0 0 0 0 recv_
F 2592 4 2588 2589 2590 2591
A 162 2 0 0 0 6 970 0 0 0 162 0 0 0 0 0 0 0 0 0
A 1013 2 0 0 800 6 1975 0 0 0 1013 0 0 0 0 0 0 0 0 0
A 1103 2 0 0 852 210 2561 0 0 0 1103 0 0 0 0 0 0 0 0 0
Z
Z
