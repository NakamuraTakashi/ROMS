      SUBROUTINE biology (ng,tile)
!
!svn $Id: fennel.h 619 2012-05-25 18:56:01Z arango $
!***********************************************************************
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                               Katja Fennel   !
!****************************************** Alexander F. Shchepetkin ***
!                                                                      !
!  This routine computes the  biological sources and sinks for the     !
!  Fennel et at. (2006) ecosystem model. Then, it adds those terms     !
!  to the global biological fields.                                    !
!                                                                      !
!  This model is loosly based on the model by Fasham et al. (1990)     !
!  but it differs in many respects.  The detailed equations of the     !
!  nitrogen cycling component  are given in  Fennel et al. (2006).     !
!  Nitrogen is the  fundamental elemental  currency in this model.     !
!  This model was adapted from a code written originally  by  John     !
!  Moisan and Emanule DiLorenzo.                                       !
!                                                                      !
!  It is recommended to activate always the  "BIO_SEDIMENT" option     !
!  to ensure conservation of mass by converting the organic matter     !
!  that is sinking out of the bottom most grid cell into inorganic     !
!  nutrients (i.e.,  instantanaous remineralization  at the water-     !
!  sediment interface). Additionally, the "DENITRIFICATION" option     !
!  can be activated.  Hence, a fraction of the instantenous bottom     !
!  remineralization is  assumed to  occur  through  the  anearobic     !
!  (denitrification)  pathway  and  thus  lost  from the  pool  of     !
!  biologically availalbe fixed nitrogen. See Fennel et al. (2006)     !
!  for details.                                                        !
!                                                                      !
!  Additional  options can be  activated to  enable  simulation of     !
!  inorganic carbon and dissolved oxygen.  Accounting of inorganic     !
!  carbon is activated by the "CARBON" option,  and results in two     !
!  additional  biological  tracer  variables:  DIC and alkalinity.     !
!  See Fennel et al. (2008) for details.                               !
!                                                                      !
!  If the "pCO2_RZ" options is activated, in addition to "CARBON",     !
!  the carbonate system  routines by Zeebe and Wolf-Gladrow (2001)     !
!  are used,  while the  OCMIP  standard routines are the default.     !
!  There are two different ways of treating alkalinity.  It can be     !
!  treated diagnostically (default),  in this case alkalinity acts     !
!  like a passive tracer  that is  not affected  by changes in the     !
!  concentration of  nitrate or ammonium.  However,  if the option     !
!  "TALK_NONCONSERV" is used,  the alkalinity  will be affected by     !
!  sources and sinks in nitrate. See Fennel et al. (2008) for more     !
!  details.                                                            !
!                                                                      !
!  If the "OXYGEN" option is activated,  one additional biological     !
!  tracer variable for dissolved oxygen. "OXYGEN" can be activated     !
!  independently of the  "CARBON"  option. If "OCMIP_OXYGEN_SC" is     !
!  used, in addition to "OXYGEN",  the Schmidt number of oxygen in     !
!  seawater will be  computed  using the  formulation  proposed by     !
!  Keeling et al. (1998, Global Biogeochem. Cycles,  12, 141-163).     !
!  Otherwise, the Wanninkhof's (1992) formula will be used.            !
!                                                                      !
!  References:                                                         !
!                                                                      !
!    Fennel, K., Wilkin, J., Levin, J., Moisan, J., O'Reilly, J.,      !
!      Haidvogel, D., 2006: Nitrogen cycling in the Mid Atlantic       !
!      Bight and implications for the North Atlantic nitrogen          !
!      budget: Results from a three-dimensional model.  Global         !
!      Biogeochemical Cycles 20, GB3007, doi:10.1029/2005GB002456.     !
!                                                                      !
!    Fennel, K., Wilkin, J., Previdi, M., Najjar, R. 2008:             !
!      Denitrification effects on air-sea CO2 flux in the coastal      !
!      ocean: Simulations for the Northwest North Atlantic.            !
!      Geophys. Res. Letters 35, L24608, doi:10.1029/2008GL036147.     !
!                                                                      !
!***********************************************************************
!
      USE mod_param
#ifdef DIAGNOSTICS_BIO
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#ifdef BBL_MODEL
      USE mod_bbl
#endif


!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
#include "tile.h"
!
!  Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15)
#endif

      CALL biology_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
#ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# if defined WET_DRY
     &                   GRID(ng) % rmask_io,                           &
# endif
#endif
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   FORCES(ng) % srflx,                            &
#ifdef BULK_FLUXES
     &                   FORCES(ng) % Uwind,                            &
     &                   FORCES(ng) % Vwind,                            &
#else
     &                   FORCES(ng) % sustr,                            &
     &                   FORCES(ng) % svstr,                            &
#endif
#ifdef CARBON_ISOTOPE
     &                   OCEAN(ng) % d13C_DIC,                          &
#endif
#ifdef DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio2d,                          &
     &                   DIAGS(ng) % DiaBio3d,                          &
#endif
     &                   GRID(ng) % p_coral,                            &
     &                   GRID(ng) % p_seagrass,                         &
     &                   GRID(ng) % p_algae,                            &
#ifdef BBL_MODEL
     &                   BBL(ng) % bustrc,                              &
     &                   BBL(ng) % bvstrc,                              &
     &                   BBL(ng) % bustrw,                              &
     &                   BBL(ng) % bvstrw,                              &
     &                   BBL(ng) % bustrcwmax,                          &
     &                   BBL(ng) % bvstrcwmax,                          &
#else
     &                   FORCES(ng) % bustr,                            &
     &                   FORCES(ng) % bvstr,                            &
#endif
     &                   OCEAN(ng) % t)


#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15)
#endif

      RETURN
      END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#ifdef MASKING
     &                         rmask,                                   &
# if defined WET_DRY
     &                         rmask_io,                                &
# endif
#endif
     &                         Hz, z_r, z_w, srflx,                     &
#ifdef BULK_FLUXES
     &                         Uwind, Vwind,                            &
#else
     &                         sustr, svstr,                            &
#endif
#ifdef CARBON_ISOTOPE
     &                         d13C_DIC,                                &
#endif
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio2d, DiaBio3d,                      &
#endif
     &                         p_coral,                                 &
     &                         p_seagrass,                              &
     &                         p_algae,                                 &
#ifdef BBL_MODEL
     &                         bustrc, bvstrc,                          &
     &                         bustrw, bvstrw,                          &
     &                         bustrcwmax, bvstrcwmax,                  &
#else
     &                         bustr, bvstr,                            &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
      
      USE mod_coral  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Coral polyp model
      USE mod_seagrass
      USE mod_geochem
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  if defined WET_DRY
      real(r8), intent(in) :: rmask_io(LBi:,LBj:)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
# else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
# endif
#ifdef CARBON_ISOTOPE
      real(r8), intent(inout) :: d13C_DIC(LBi:,LBj:,:)
#endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
      real(r8), intent(inout) :: p_coral(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: p_seagrass(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: p_algae(LBi:UBi,LBj:UBj)
#  ifdef BBL_MODEL
      real(r8), intent(in) :: bustrc(LBi:,LBj:)
      real(r8), intent(in) :: bvstrc(LBi:,LBj:)
      real(r8), intent(in) :: bustrw(LBi:,LBj:)
      real(r8), intent(in) :: bvstrw(LBi:,LBj:)
      real(r8), intent(in) :: bustrcwmax(LBi:,LBj:)
      real(r8), intent(in) :: bvstrcwmax(LBi:,LBj:)
#  else
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
#  endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)

#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  if defined WET_DRY
      real(r8), intent(in) :: rmask_io(LBi:UBi,LBj:UBj)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
# endif
#ifdef CARBON_ISOTOPE
      real(r8), intent(inout) :: d13C_DIC(LBi:UBi,LBj:UBj,UBk)
#endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
# endif
      real(r8), intent(inout) :: p_coral(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: p_seagrass(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: p_algae(LBi:UBi,LBj:UBj)
#  ifdef BBL_MODEL
      real(r8), intent(in) :: bustrc(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrc(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bustrw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bustrcwmax(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrcwmax(LBi:UBi,LBj:UBj)
#  else
      real(r8), intent(in) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstr(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!

      integer, parameter :: Nsink = 6  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<—vŒŸ“¢
      real(r8) :: u10squ


      integer :: Iter, i, ibio, isink, itrc, ivar, j, k, ks

      integer, dimension(Nsink) :: idsink

      real(r8), parameter :: eps = 1.0e-20_r8

! air-sea O2 flux
      real(r8), parameter :: OA0 = 2.00907_r8       ! Oxygen
      real(r8), parameter :: OA1 = 3.22014_r8       ! saturation
      real(r8), parameter :: OA2 = 4.05010_r8       ! coefficients
      real(r8), parameter :: OA3 = 4.94457_r8
      real(r8), parameter :: OA4 =-0.256847_r8
      real(r8), parameter :: OA5 = 3.88767_r8
      real(r8), parameter :: OB0 =-0.00624523_r8
      real(r8), parameter :: OB1 =-0.00737614_r8
      real(r8), parameter :: OB2 =-0.0103410_r8
      real(r8), parameter :: OB3 =-0.00817083_r8
      real(r8), parameter :: OC0 =-0.000000488682_r8
      real(r8), parameter :: rOxNO3= 8.625_r8       ! 138/16
      real(r8), parameter :: rOxNH4= 6.625_r8       ! 106/16
      real(r8) :: l2mol = 1000.0_r8/22.9316_r8      ! liter to mol

! air-sea CO2 flux
      integer :: iday, month, year

      integer, parameter :: DoNewton = 0            ! pCO2 solver

      real(r8), parameter :: Acoef = 2073.1_r8      ! Schmidt
      real(r8), parameter :: Bcoef = 125.62_r8      ! number
      real(r8), parameter :: Ccoef = 3.6276_r8      ! transfer
      real(r8), parameter :: Dcoef = 0.043219_r8    ! coefficients

      real(r8), parameter :: A1 = -60.2409_r8       ! surface
      real(r8), parameter :: A2 = 93.4517_r8        ! CO2
      real(r8), parameter :: A3 = 23.3585_r8        ! solubility
      real(r8), parameter :: B1 = 0.023517_r8       ! coefficients
      real(r8), parameter :: B2 = -0.023656_r8
      real(r8), parameter :: B3 = 0.0047036_r8

      real(r8) :: pmonth                         ! months since Jan 1951
      real(r8) :: pCO2air_secular
      real(r8) :: yday, hour

      real(r8), parameter :: pi2 = 6.2831853071796_r8

      real(r8), parameter :: D0 = 282.6_r8          ! coefficients
      real(r8), parameter :: D1 = 0.125_r8          ! to calculate
      real(r8), parameter :: D2 =-7.18_r8           ! secular trend in
      real(r8), parameter :: D3 = 0.86_r8           ! atmospheric pCO2
      real(r8), parameter :: D4 =-0.99_r8
      real(r8), parameter :: D5 = 0.28_r8
      real(r8), parameter :: D6 =-0.80_r8
      real(r8), parameter :: D7 = 0.06_r8
!----------

      real(r8) :: Att, AttFac, ExpAtt, Itop
      real(r8) :: Epp, L_NH4, L_NO3, LTOT, Vp
      real(r8) :: Chl2C, dtdays, t_PPmax, inhNH4
      real(r8) :: dtc
      real(r8) :: Phot, Resp, Calc
      
      real(r8) :: cff, cff1, cff2, cff3, cff4, cff5
      real(r8) :: fac1, fac2, fac3
      real(r8) :: cffL, cffR, cu, dltL, dltR

      real(r8) :: total_N

      real(r8) :: SchmidtN_Ox, O2satu, O2_Flux
      real(r8) :: TS, AA

      real(r8) :: C_Flux_RemineL, C_Flux_RemineS
      real(r8) :: CO2_Flux, CO2_sol, SchmidtN, TempK
      real(r8) :: pH, cCO2aq,cHCO3,cCO3,fCO2,Warg

      real(r8) :: N_Flux_Assim
      real(r8) :: N_Flux_CoagD, N_Flux_CoagP
      real(r8) :: N_Flux_Egest
      real(r8) :: N_Flux_NewProd, N_Flux_RegProd
      real(r8) :: N_Flux_Nitrifi
      real(r8) :: N_Flux_Pmortal, N_Flux_Zmortal
      real(r8) :: N_Flux_RemineL, N_Flux_RemineS
      real(r8) :: N_Flux_Zexcret, N_Flux_Zmetabo

      real(r8), dimension(Nsink) :: Wbio

      integer, dimension(IminS:ImaxS,N(ng)) :: ksource

      real(r8), dimension(IminS:ImaxS) :: PARsur
      real(r8), dimension(IminS:ImaxS) :: PAR
!      real(r8), dimension(IminS:ImaxS) :: pCO2

      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio_old

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC

      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: qc

      real(r8), dimension(IminS:ImaxS) :: tau_w      
      real(r8) Flx(NT(ng))

#include "set_bounds.h"


#ifdef DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
! If appropriate, initialize time-averaged diagnostic arrays.
!-----------------------------------------------------------------------
!

      IF (((iic(ng).gt.ntsDIA(ng)).and.                                 &
     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                            &
     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.             &
     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
        DO ivar=1,NDbio2d
          DO j=Jstr,Jend
            DO i=Istr,Iend
              DiaBio2d(i,j,ivar)=0.0_r8
            END DO
          END DO
        END DO
        DO ivar=1,NDbio3d
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend
                DiaBio3d(i,j,k,ivar)=0.0_r8
              END DO
            END DO
          END DO
        END DO
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Add biological Source/Sink terms.
!-----------------------------------------------------------------------
!
!  Avoid computing source/sink terms if no biological iterations.
!
      IF (BioIter(ng).le.0) RETURN
!
!  Set time-stepping according to the number of iterations.
!
      dtdays=dt(ng)*sec2day!/REAL(BioIter(ng),r8)
      dtc=dt(ng)/REAL(BioIter(ng),r8) !sec
!
!  Set vertical sinking indentification vector.
!
!      idsink(1)=iPhyt
!      idsink(2)=iChlo
!      idsink(3)=iSDeN
!      idsink(4)=iLDeN
!      idsink(5)=iSDeC
!      idsink(6)=iLDeC
!!
!!  Set vertical sinking velocity vector in the same order as the
!!  identification vector, IDSINK.
!!
!      Wbio(1)=wPhy(ng)                ! phytoplankton
!      Wbio(2)=wPhy(ng)                ! chlorophyll
!      Wbio(3)=wSDet(ng)               ! small Nitrogen-detritus
!      Wbio(4)=wLDet(ng)               ! large Nitrogen-detritus
!      Wbio(5)=wSDet(ng)               ! small Carbon-detritus
!      Wbio(6)=wLDet(ng)               ! large Carbon-detritus
!
!  Compute inverse thickness to avoid repeated divisions.
!
      J_LOOP : DO j=Jstr,Jend

        DO k=1,N(ng)
          DO i=Istr,Iend
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
        DO k=1,N(ng)-1
          DO i=Istr,Iend
            Hz_inv2(i,k)=1.0_r8/(Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
        DO k=2,N(ng)-1
          DO i=Istr,Iend
            Hz_inv3(i,k)=1.0_r8/(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
!
!  Extract biological variables from tracer arrays, place them into
!  scratch arrays, and restrict their values to be positive definite.
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio_old(i,k,ibio)=MAX(0.0_r8,t(i,j,k,nstp,ibio))
              Bio(i,k,ibio)=Bio_old(i,k,ibio)
            END DO
          END DO
        END DO

!        DO k=1,N(ng)
!          DO i=Istr,Iend
!            Bio_old(i,k,iTIC_)=MIN(Bio_old(i,k,iTIC_),3000.0_r8)
!            Bio_old(i,k,iTIC_)=MAX(Bio_old(i,k,iTIC_),400.0_r8)
!            Bio(i,k,iTIC_)=Bio_old(i,k,iTIC_)
!          END DO
!        END DO

!
!  Extract potential temperature and salinity.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio(i,k,itemp)=MIN(t(i,j,k,nstp,itemp),40.0_r8)
            Bio(i,k,isalt)=MAX(t(i,j,k,nstp,isalt), 0.0_r8)
          END DO
        END DO
!
!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
        DO i=Istr,Iend
          PARsur(i)=PARfrac(ng)*srflx(i,j)*rho0*Cp
        END DO
!
!=======================================================================
!  Start internal iterations to achieve convergence of the nonlinear
!  backward-implicit solution.
!=======================================================================
!
!  During the iterative procedure a series of fractional time steps are
!  performed in a chained mode (splitting by different biological
!  conversion processes) in sequence of the main food chain.  In all
!  stages the concentration of the component being consumed is treated
!  in fully implicit manner, so the algorithm guarantees non-negative
!  values, no matter how strong s the concentration of active consuming
!  component (Phytoplankton or Zooplankton).  The overall algorithm,
!  as well as any stage of it, is formulated in conservative form
!  (except explicit sinking) in sense that the sum of concentration of
!  all components is conserved.
!
!
!  In the implicit algorithm, we have for example (N: nitrate,
!                                                  P: phytoplankton),
!
!     N(new) = N(old) - uptake * P(old)     uptake = mu * N / (Kn + N)
!                                                    {Michaelis-Menten}
!  below, we set
!                                           The N in the numerator of
!     cff = mu * P(old) / (Kn + N(old))     uptake is treated implicitly
!                                           as N(new)
!
!  so the time-stepping of the equations becomes:
!
!     N(new) = N(old) / (1 + cff)     (1) when substracting a sink term,
!                                         consuming, divide by (1 + cff)
!  and
!
!     P(new) = P(old) + cff * N(new)  (2) when adding a source term,
!                                         growing, add (cff * source)
!
!  Notice that if you substitute (1) in (2), you will get:
!
!     P(new) = P(old) + cff * N(old) / (1 + cff)    (3)
!
!  If you add (1) and (3), you get
!
!     N(new) + P(new) = N(old) + P(old)
!
!  implying conservation regardless how "cff" is computed. Therefore,
!  this scheme is unconditionally stable regardless of the conversion
!  rate. It does not generate negative values since the constituent
!  to be consumed is always treated implicitly. It is also biased
!  toward damping oscillations.
!
!  The iterative loop below is to iterate toward an universal Backward-
!  Euler treatment of all terms. So if there are oscillations in the
!  system, they are only physical oscillations. These iterations,
!  however, do not improve the accuaracy of the solution.
!
!        ITER_LOOP: DO Iter=1,BioIter(ng)
!
!-----------------------------------------------------------------------
!  Light-limited computations.
!-----------------------------------------------------------------------
!
!  Compute attenuation coefficient based on the concentration of
!  chlorophyll-a within each grid box.  Then, attenuate surface
!  photosynthetically available radiation (PARsur) down inot the
!  water column.  Thus, PAR at certain depth depends on the whole
!  distribution of chlorophyll-a above.
!  To compute rate of maximum primary productivity (t_PPmax), one needs
!  PAR somewhat in the middle of the gridbox, so that attenuation "Att"
!  corresponds to half of the grid box height, while PAR is multiplied
!  by it twice: once to get it in the middle of grid-box and once the
!  compute on the lower grid-box interface.
!
          DO i=Istr,Iend
            PAR(i)=PARsur(i)
            AttFac=0.0_r8
            DO k=N(ng),1,-1
#ifdef NUTRIENTS
!
!  Compute average light attenuation for each grid cell. To include
!  other attenuation contributions like suspended sediment or CDOM
!  modify AttFac.
!
                Att=(AttSW(ng)+                                         &
     &               AttChl(ng)*Bio(i,k,iChlo)+                         &
     &               AttFac)*                                           &
     &               (z_w(i,j,k)-z_w(i,j,k-1))
                ExpAtt=EXP(-Att)
                Itop=PAR(i)
                PAR(i)=Itop*(1.0_r8-ExpAtt)/Att    ! average at cell center
!
!  Compute Chlorophyll-a phytoplankton ratio, [mg Chla / (mg C)].
!
                cff=PhyCN(ng)*12.0_r8
                Chl2C=MIN(Bio(i,k,iChlo)/(Bio(i,k,iPhyt)*cff+eps),      &
     &                    Chl2C_m(ng))

#else
                Att= AttSW(ng)*                                   &
     &               (z_w(i,j,k)-z_w(i,j,k-1))
                ExpAtt=EXP(-Att)
                Itop=PAR(i)
                PAR(i)=Itop*(1.0_r8-ExpAtt)/Att    ! average at cell center
#endif







!
!  Light attenuation at the bottom of the grid cell. It is the starting
!  PAR value for the next (deeper) vertical grid cell.
!
              PAR(i)=Itop*ExpAtt
              
              DiaBio2d(i,j,iPARb) =PAR(i)

            END DO
          END DO
          
!-----------------------------------------------------------------------
          
          DO i=Istr,Iend
!!!---------------------------------------------------------------------
!!!  Coral polyo model
!!!---------------------------------------------------------------------
            IF(p_coral(i,j) .gt. 0.0_r8) THEN
!
!-----------------------------------------------------------------------
! Compute bottom shear stress (N m-2).
!-----------------------------------------------------------------------
!
#ifdef BBL_MODEL
              tau_w(i)=SQRT(bustrcwmax(i,j)*bustrcwmax(i,j)+          &
     &                      bvstrcwmax(i,j)*bvstrcwmax(i,j)) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)
#else
              tau_w(i)=0.5_r8*SQRT((bustr(i,j)+bustr(i+1,j))*         &
     &                             (bustr(i,j)+bustr(i+1,j))+         &
     &                             (bvstr(i,j)+bvstr(i,j+1))*         &
     &                             (bvstr(i,j)+bvstr(i,j+1))) *rho0
#endif
#ifdef WET_DRY
              tau_w(i)=tau_w(i)*rmask_io(i,j)
#endif
!-----------------------------------------------------------------------
! Compute coral polyp model.
!-----------------------------------------------------------------------

              ITER_LOOP: DO Iter=1,BioIter(ng)   !!! Loop for coral polyp model: dtc <= 0.05 sec
!
                CALL coral_polyp         &
!          input parameters
     &            (1,i,j             &   ! n: coral compartment; i,j: position
     &            ,dtc             &   ! Time step (sec)
     &            ,PAR(i)            &   ! Photon flux density (umol m-2 s-1)
     &            ,Bio(i,1,itemp)          &   ! Temperature (oC)
     &            ,Bio(i,1,isalt)          &   ! Salinity (PSU)
     &            ,Bio(i,1,iTIC_)          &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,Bio(i,1,iTAlk)          &   ! Total alkalinity (TA: umol kg-1)
     &            ,Bio(i,1,iOxyg)          &   ! Dissolved oxygen (umol L-1)
#if defined NUTRIENTS
     &            ,Bio(i,1,iNO3_)         &   ! NO3 (umol L-1)
     &            ,Bio(i,1,iNO2_)         &   ! NO2 (umol L-1)
     &            ,Bio(i,1,iNH4_)         &   ! NH4 (umol L-1)
     &            ,Bio(i,1,iPO4_)         &   ! PO4 (umol L-1)
     &            ,Bio(i,1,iSDeC)         &   ! Dissolved organic carbon (DOC: umol L-1)
     &            ,Bio(i,1,iSDeN)         &   ! Dissolved organic nitrogen (DON: umol L-1)
     &            ,Bio(i,1,iSDeP)         &   ! Dissolved organic phosporius (DOP: umol L-1)
     &            ,Bio(i,1,iPhyt)         &   ! phytoplankton (umol C L-1)
     &            ,Bio(i,1,iZoop)         &   ! zooplankton (umol C L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,Bio(i,1,iT13C)       &   !13C of DIC (umol kg-1)
#endif
     &            ,tau_w(i)        &   ! bottom shear stress (N m-2)
     &            ,0.0_r8           &   ! sedimentation rate (??)
!          output parameters
     &            ,Phot             &   ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,Resp             &   ! Respiration rate (nmol cm-2 s-1)
     &            ,Calc             &   ! Galcification rate (nmol cm-2 s-1)
     &            ,Flx(iTIC_)      &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iTAlk)       &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iOxyg)       &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#if defined NUTRIENTS
     &            ,Flx(iNO3_)      &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iNO2_)      &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iNH4_)      &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iPO4_)      &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iSDeC)      &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,Flx(iSDeN)      &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,Flx(iSDeP)      &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,Flx(iPhyt)      &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iZoop)      &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CARBON_ISOTOPE
     &            ,Flx(iT13C)    &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
     &             )

              END DO ITER_LOOP

              ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
              ! cff: convaert [nmol cm-2 s-1] to [umol L-1]

              cff=0.01_r8*Hz_inv(i,1)*dt(ng) * p_coral(i,j)*10.0_r8  !!!*40.0_r8 ‚Ä‚«‚Æ‚¤ convert projected area to coral surface area
               
              Bio(i,1,iTIC_) = Bio(i,1,iTIC_) - Flx(iTIC_)*cff/1.024_r8
              Bio(i,1,iTAlk) = Bio(i,1,iTAlk) - Flx(iTAlk)*cff/1.024_r8
              Bio(i,1,iOxyg) = Bio(i,1,iOxyg) - Flx(iOxyg)*cff
#if defined NUTRIENTS
              Bio(i,1,iNO3_) = Bio(i,1,iNO3_) - Flx(iNO3_)*cff
              Bio(i,1,iNO2_) = Bio(i,1,iNO2_) - Flx(iNO2_)*cff
              Bio(i,1,iNH4_) = Bio(i,1,iNH4_) - Flx(iNH4_)*cff
              Bio(i,1,iPO4_) = Bio(i,1,iPO4_) - Flx(iPO4_)*cff
              Bio(i,1,iSDeC) = Bio(i,1,iSDeC) - Flx(iSDeC)*cff
              Bio(i,1,iSDeN) = Bio(i,1,iSDeN) - Flx(iSDeN)*cff
              Bio(i,1,iSDeP) = Bio(i,1,iSDeP) - Flx(iSDeP)*cff
              Bio(i,1,iPhyt) = Bio(i,1,iPhyt) - Flx(iPhyt)*cff
              Bio(i,1,iZoop) = Bio(i,1,iZoop) - Flx(iZoop)*cff
#endif
#if defined CARBON_ISOTOPE
              Bio(i,1,iT13C) = Bio(i,1,iT13C) - Flx(iT13C)*cff/1.024_r8
#endif
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
              DiaBio2d(i,j,iClPg) =Phot
              DiaBio2d(i,j,iCl_R) =Resp
              DiaBio2d(i,j,iClPn) =Phot-Resp
              DiaBio2d(i,j,iCl_G) =Calc
#endif
            END IF

!!!---------------------------------------------------------------------
!!!  Seagrass model
!!!---------------------------------------------------------------------
            IF(p_seagrass(i,j) .gt. 0.0_r8) THEN
              CALL seagrass       &
!          input parameters
     &            (1,i,j             &   ! n: seagrass compartment; i,j: position
     &            ,PAR(i)            &   ! Photon flux density (umol m-2 s-1)
     &            ,Bio(i,1,iTIC_)         &   ! DIC (umol kg-1)
#if defined NUTRIENTS         
     &            ,Bio(i,1,iNH4_)         &   ! NH4 concentration (umol L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,Bio(i,1,iT13C)       &   ! 13C of DIC (umol kg-1)
#endif
!          output parameters
     &            ,Phot             &   ! Gross photosynthesis rate (mmol m-2 s-1)
     &            ,Resp              &   ! Respiration rate (mmol m-2 s-1)
     &            ,Flx(iTIC_)      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iOxyg)       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#if defined NUTRIENTS         
     &            ,Flx(iNO3_)      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iNH4_)      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flx(iPO4_)      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CARBON_ISOTOPE
     &            ,Flx(iT13C)    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
     &             )

              ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
              ! cff: convaert [nmol cm-2 s-1] to [umol L-1]

              cff=Hz_inv(i,1)*dt(ng) * p_seagrass(i,j)

              Bio(i,1,iTIC_) = Bio(i,1,iTIC_) - Flx(iTIC_)*cff/1.024_r8
              Bio(i,1,iOxyg) = Bio(i,1,iOxyg) - Flx(iOxyg)*cff
#if defined NUTRIENTS
              Bio(i,1,iNO3_) = Bio(i,1,iNO3_) - Flx(iNO3_)*cff
              Bio(i,1,iNH4_) = Bio(i,1,iNH4_) - Flx(iNH4_)*cff
              Bio(i,1,iPO4_) = Bio(i,1,iPO4_) - Flx(iPO4_)*cff
#endif
#if defined CARBON_ISOTOPE
              Bio(i,1,iT13C) = Bio(i,1,iT13C) - Flx(iT13C)*cff/1.024_r8
#endif
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
              DiaBio2d(i,j,iSgPg) =Phot
              DiaBio2d(i,j,iSg_R) =Resp
              DiaBio2d(i,j,iSgPn) =Phot-Resp

#endif

            END IF

!!!---------------------------------------------------------------------
!!!  Algae model
!!!---------------------------------------------------------------------
!            IF(p_algae(i,j) .gt. 0.0_r8) THEN
             ! call algae model
!            END IF            
          END DO
!
!-----------------------------------------------------------------------
!  Surface O2 gas exchange.
!-----------------------------------------------------------------------
!
!  Compute surface O2 gas exchange.
!
          cff1=rho0*550.0_r8
          cff2=dtdays*0.31_r8*24.0_r8/100.0_r8
          k=N(ng)
          DO i=Istr,Iend
!
!  Compute O2 transfer velocity : u10squared (u10 in m/s)
!
# ifdef BULK_FLUXES
            u10squ=Uwind(i,j)*Uwind(i,j)+Vwind(i,j)*Vwind(i,j)
# else
            u10squ=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+     &
     &                       (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
# endif
# ifdef OCMIP_OXYGEN_SC
!
!  Alternative formulation for Schmidt number (Sc will be slightly
!  smaller up to about 35 C): Compute the Schmidt number of oxygen
!  in seawater using the formulation proposed by Keeling et al.
!  (1998, Global Biogeochem. Cycles, 12, 141-163).  Input temperature
!  in Celsius.
!
            SchmidtN_Ox=1638.0_r8-                                      &
     &                  Bio(i,k,itemp)*(81.83_r8-                       &
     &                                  Bio(i,k,itemp)*                 &
     &                                  (1.483_r8-                      &
     &                                   Bio(i,k,itemp)*0.008004_r8))
# else
!
!  Calculate the Schmidt number for O2 in sea water (Wanninkhof, 1992).
!
            SchmidtN_Ox=1953.4_r8-                                      &
     &                  Bio(i,k,itemp)*(128.0_r8-                       &
     &                                  Bio(i,k,itemp)*                 &
     &                                  (3.9918_r8-                     &
     &                                   Bio(i,k,itemp)*0.050091_r8))
# endif

            cff3=cff2*u10squ*SQRT(660.0_r8/SchmidtN_Ox)
!
!  Calculate O2 saturation concentration using Garcia and Gordon
!  L&O (1992) formula, (EXP(AA) is in ml/l).
!
            TS=LOG((298.15_r8-Bio(i,k,itemp))/                          &
     &             (273.15_r8+Bio(i,k,itemp)))
            AA=OA0+TS*(OA1+TS*(OA2+TS*(OA3+TS*(OA4+TS*OA5))))+          &
     &             Bio(i,k,isalt)*(OB0+TS*(OB1+TS*(OB2+TS*OB3)))+       &
     &             OC0*Bio(i,k,isalt)*Bio(i,k,isalt)
!
!  Convert from ml/l to mmol/m3.
!
            O2satu=l2mol*EXP(AA)
!
!  Add in O2 gas exchange.
!
            O2_Flux=cff3*(O2satu-Bio(i,k,iOxyg))
            Bio(i,k,iOxyg)=Bio(i,k,iOxyg)+                              &
     &                     O2_Flux*Hz_inv(i,k)
# ifdef DIAGNOSTICS_BIO
            DiaBio2d(i,j,iO2fx)=DiaBio2d(i,j,iO2fx)+                    &
#  ifdef WET_DRY
     &                          rmask_io(i,j)*                          &
#  endif
     &                          O2_Flux
# endif

          END DO

#ifdef NUTRIENTS
!
!-----------------------------------------------------------------------
!  Allow different remineralization rates for detrital C and detrital N.
!-----------------------------------------------------------------------
!
          cff1=dtdays*SDeRRC(ng)
          cff2=1.0_r8/(1.0_r8+cff1)
          cff3=dtdays*LDeRRC(ng)
          cff4=1.0_r8/(1.0_r8+cff3)
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio(i,k,iSDeC)=Bio(i,k,iSDeC)*cff2
              Bio(i,k,iLDeC)=Bio(i,k,iLDeC)*cff4
              C_Flux_RemineS=Bio(i,k,iSDeC)*cff1
              C_Flux_RemineL=Bio(i,k,iLDeC)*cff3
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                            &
     &                       C_Flux_RemineS+C_Flux_RemineL
            END DO
          END DO
#endif
!
!-----------------------------------------------------------------------
!  Surface CO2 gas exchange.
!-----------------------------------------------------------------------

          k=N(ng)

          DO i=Istr,Iend
#if defined MASKING
            IF(rmask(i,j) .gt. 0.0_r8) THEN
#endif
!
!  Compute CO2 transfer velocity : u10squared (u10 in m/s)
!
#ifdef BULK_FLUXES
              u10squ=Uwind(i,j)**2+Vwind(i,j)**2
#else
              u10squ=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+     &
     &                       (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
#endif
!
!  Compute surface CO2 gas exchange.
!
!----------- CO2 system in ambient seawater -------------------
              TempK= Bio(i,k,itemp)+273.15_r8
              pH   = pH_fromATCT( Bio(i,k,iTAlk), Bio(i,k,iTIC_),TempK, Bio(i,k,isalt))
              cCO2aq = cCO2aq_fromCTpH(Bio(i,k,iTIC_),pH,TempK, Bio(i,k,isalt))
!              cHCO3 = cHCO3_fromCTpH(Bio(i,k,iTIC_),pH,TempK, Bio(i,k,isalt))
              cCO3 = cCO3_fromCTpH(Bio(i,k,iTIC_),pH,TempK, Bio(i,k,isalt))
              fCO2 = fCO2_fromcCO2aq(cCO2aq,TempK, Bio(i,k,isalt))  !! for output
              Warg = Warg_fromcCO3(cCO3,TempK, Bio(i,k,isalt))

              CO2_Flux=Flux_CO2(fCO2, pCO2air(ng), u10squ, TempK, Bio(i,k,isalt))  ! sea to air is positive
!              CO2_Flux=0.d0
!              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)-CO2_Flux*Hz_inv(i,k)
!              Bio(i,k,iT13C)=Bio(i,k,iT13C)-CO2_Flux*Hz_inv(i,k)
#ifdef DIAGNOSTICS_BIO
              DiaBio2d(i,j,iCOfx)=CO2_Flux
              DiaBio2d(i,j,ipHt_)=pH
              DiaBio2d(i,j,iWarg)=Warg
              DiaBio2d(i,j,ipCO2)=fCO2
#endif
#if defined MASKING
            END IF
#endif
          END DO

!
!-----------------------------------------------------------------------
!  Vertical sinking terms.
!-----------------------------------------------------------------------
!
!  Reconstruct vertical profile of selected biological constituents
!  "Bio(:,:,isink)" in terms of a set of parabolic segments within each
!  grid box. Then, compute semi-Lagrangian flux due to sinking.
!

!        END DO ITER_LOOP
!
!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of N and
!  C even when advection causes tracer concentration to go negative.
!  (J. Wilkin and H. Arango, Apr 27, 2012)
!-----------------------------------------------------------------------
!

        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff=Bio(i,k,ibio)-Bio_old(i,k,ibio)
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)+cff*Hz(i,j,k)
            END DO
          END DO
        END DO
        
! Carbon isotope ratio calculation

        DO k=1,N(ng)
          DO i=Istr,Iend
            d13C_DIC(i,j,k)=d13C_fromR13C(t(i,j,k,nnew,iT13C)/t(i,j,k,nnew,iTIC_))
          END DO
        END DO
        
      END DO J_LOOP

      RETURN
      END SUBROUTINE biology_tile

