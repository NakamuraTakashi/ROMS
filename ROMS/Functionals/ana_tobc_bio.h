      SUBROUTINE ana_tobc_bio (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets tracer-type variables open boundary conditions    !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_boundary
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_tobc_bio_tile (ng, tile, model,                              &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nstp(ng),                                     &
     &                    GRID(ng) % z_r,                               &
     &                    OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(34)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_tobc_bio
!
!***********************************************************************
      SUBROUTINE ana_tobc_bio_tile (ng, tile, model,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nstp,                                   &
     &                          z_r, t)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_boundary
      USE mod_ncparam
      USE mod_ocean
#ifdef SEDIMENT
      USE mod_sediment
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#ifdef REEF_ECOSYS
      USE mod_biology
      USE mod_geochem
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp

#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#else
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, ised, itrc, j, k
      real(r8) :: cff

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Tracers open boundary conditions.
!-----------------------------------------------------------------------
!
#ifdef SHIRAHO_REEF
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_east(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_east(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_east(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_east(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_east(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_east(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif

          END DO
        END DO
      END IF
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_west(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_west(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_west(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_west(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_west(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_west(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_west(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_south(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_south(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_south(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_south(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_south(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_south(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_south(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_north(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_north(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_north(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_north(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_north(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_north(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_north(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF
      
! YAEYAMA2 case
#elif defined YAEYAMA2
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_east(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_east(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_east(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_east(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_east(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_east(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif

          END DO
        END DO
      END IF
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_west(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_west(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_west(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_west(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_west(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_west(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_west(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_south(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_south(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_south(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_south(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_south(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_south(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_south(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_north(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_north(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_north(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_north(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_north(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_north(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_north(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#else
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_east(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_west(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_south(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_north(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF
#endif

      RETURN
      END SUBROUTINE ana_tobc_bio_tile
