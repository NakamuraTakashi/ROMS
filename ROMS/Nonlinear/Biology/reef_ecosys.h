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
     &                   OCEAN(ng) % HisBio2d,                          &
     &                   OCEAN(ng) % HisBio3d,                          &
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
# ifdef SSW_BBL
     &                   BBL(ng) % bstrcwave,                           &
# endif
#else
     &                   FORCES(ng) % bustr,                            &
     &                   FORCES(ng) % bvstr,                            &
#endif
     &                   FORCES(ng) % Dwave,                            &
     &                   FORCES(ng) % Pwave_bot,                        &
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
     &                         HisBio2d, HisBio3d,                      &
     &                         p_coral,                                 &
     &                         p_seagrass,                              &
     &                         p_algae,                                 &
#ifdef BBL_MODEL
     &                         bustrc, bvstrc,                          &
     &                         bustrw, bvstrw,                          &
     &                         bustrcwmax, bvstrcwmax,                  &
# ifdef SSW_BBL
     &                         bstrcwave,                               &
# endif
#else
     &                         bustr, bvstr,                            &
#endif
     &                         Dwave, Pwave_bot,                        &
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
      
      USE mod_reef_ecosys  !!!<<<<<<<<<<<<<<<< Reef ecosystem model
      USE mod_geochem
      USE mod_coral
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
      real(r8), intent(inout) :: HisBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: HisBio3d(LBi:,LBj:,:,:)
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
#   ifdef SSW_BBL
      real(r8), intent(in) :: bstrcwave(LBi:,LBj:)
#   endif
#  else
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: Dwave(LBi:,LBj:)
      real(r8), intent(in) :: Pwave_bot(LBi:,LBj:)
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
      real(r8), intent(inout) :: HisBio2d(LBi:UBi,LBj:UBj,NHbio2d)
      real(r8), intent(inout) :: HisBio3d(LBi:UBi,LBj:UBj,UBk,NHbio3d)
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
#   ifdef SSW_BBL
      real(r8), intent(in) :: bstrcwave(LBi:UBi,LBj:UBj)
#   endif
#  else
      real(r8), intent(in) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstr(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: Dwave(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Pwave_bot(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!
      integer :: Iter, i, ibio, isink, itrc, ivar, j, k, ks

      real(r8) :: PFDsurf    
      real(r8) :: tau, tau_u, tau_v        
      real(r8) :: u10        
      
      real(r8) :: sspH      
      real(r8) :: ssfCO2    
      real(r8) :: ssWarg    
      real(r8) :: ssCO2flux 
      real(r8) :: ssO2flux  
      real(r8) :: PFDbott   
      real(r8) :: coral_Pg  
      real(r8) :: coral_R   
      real(r8) :: coral_Gn  
      real(r8) :: sgrass_Pg 
      real(r8) :: sgrass_R  
#ifdef CORAL_CARBON_ISOTOPE
      real(r8) :: R13CH2O
#endif
      real(r8) :: dtrc_dt(UBk,UBt)

#include "set_bounds.h"

          
!$acc kernels
!-----------------------------------------------------------------------
      DO j=Jstr,Jend

        DO i=Istr,Iend
! Set initial zero 
          DO k=1,N(ng)
            DO itrc=1,NBT
              ibio=idbio(itrc)
              dtrc_dt(k,ibio)=0.0_r8
            END DO
          END DO
          sspH      = 8.0_r8         ! sea surface pH
          ssfCO2    = 380.0_r8         ! sea surface fCO2 (uatm)
          ssWarg    = 4.0_r8         ! sea surface aragonite saturation state
          ssCO2flux = 0.0_r8         ! sea surface CO2 flux (mmol m-2 s-1)
          ssO2flux  = 0.0_r8         ! sea surface O2 flux (mmol m-2 s-1)
          coral_Pg  = 0.0_r8         ! Coral gross photosynthesis rate (nmol cm-2 s-1)
          coral_R   = 0.0_r8         ! Coral respiration rate (nmol cm-2 s-1)
          coral_Gn  = 0.0_r8         ! Coral calcification rate (nmol cm-2 s-1)
          sgrass_Pg = 0.0_r8         ! seagrass gross photosynthesis rate (mmol m-2 s-1)
          sgrass_R  = 0.0_r8         ! seagrass respiration rate (mmol m-2 s-1)

#ifdef MASKING
          IF (rmask(i,j).eq.1.0_r8) THEN
# endif

!
!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
            PFDsurf = PARfrac(ng)*srflx(i,j)*rho0*Cp

            u10 = SQRT( Uwind(i,j)*Uwind(i,j) + Vwind(i,j)*Vwind(i,j) )
!
!-----------------------------------------------------------------------
! Compute bottom shear stress (N m-2).
!-----------------------------------------------------------------------

#ifdef BBL_MODEL
# ifdef SSW_BBL
            tau = bstrcwave(i,j) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)
# else
            tau_u=bustrc(i,j)+0.5_r8*bustrw(i,j)
            tau_v=bvstrc(i,j)+0.5_r8*bvstrw(i,j)
            tau = SQRT(tau_u*tau_u + tau_v*tau_v) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)
            
!            tau = SQRT(bustrcwmax(i,j)*bustrcwmax(i,j)+          &
!     &                   bvstrcwmax(i,j)*bvstrcwmax(i,j)) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)

# endif
#else
            tau = 0.5_r8*SQRT((bustr(i,j)+bustr(i+1,j))*         &
     &                          (bustr(i,j)+bustr(i+1,j))+         &
     &                          (bvstr(i,j)+bvstr(i,j+1))*         &
     &                          (bvstr(i,j)+bvstr(i,j+1))) *rho0
#endif
#ifdef WET_DRY
            tau = tau*rmask_io(i,j)
#endif

!----- Ecosystem model ----------------------------------------

            CALL reef_ecosys          &
!          input parameters
     &            (ng, i, j            &   ! ng: nested grid number; i,j: position
     &            ,N(ng)               &   ! Number of vertical grid (following ROMS vertical grid)
     &            ,CrlIter(ng)         &   ! Internal loop counts of coral polyp model
     &            ,SedIter(ng)         &   ! Internal loop counts of sediment ecosystem model
     &            ,dt(ng)              &   ! Time step (sec)
     &            ,Hz(i,j,:)           &   ! dz(N): vertical grid size (m)
     &            ,PFDsurf             &   ! Sea surface photon flux density (umol m-2 s-1)
     &            ,tau                 &   ! bottom shear stress (N m-2)
     &            ,pCO2air(ng)         &   ! Air CO2 pertial pressure (uatm)
     &            ,u10                 &   ! wind speed (m s-1)

     &            ,p_coral(i,j)        &   ! Coral coverage (0-1)
     &            ,p_seagrass(i,j)     &   ! seagrass coverage (0-1)
     &            ,p_algae(i,j)        &   ! algal coverage (0-1)
     &            ,1.0_r8              &   ! sediment coverage (0-1)

     &            ,t(i,j,:,nstp,iTemp)     &   ! Tmp(N): Temperature (oC)
     &            ,t(i,j,:,nstp,iSalt)     &   ! Sal(N): Salinity (PSU)
     &            ,t(i,j,:,nstp,iTIC_)     &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,t(i,j,:,nstp,iTAlk)     &   ! TA (N): Total alkalinity (TA: umol kg-1)
     &            ,t(i,j,:,nstp,iOxyg)     &   ! DOx(N): Dissolved oxygen (umol L-1)
#if defined ORGANIC_MATTER
     &            ,t(i,j,:,nstp,iDOC_)     &   ! DOC(N): Dissolved organic carbon (DOC: umol L-1)
     &            ,t(i,j,:,nstp,iPOC_)     &   ! POC(N): Particulate organic carbon (POC: umol L-1)
     &            ,t(i,j,:,nstp,iPhyt)     &   ! PHY(N): phytoplankton (umol C L-1)
     &            ,t(i,j,:,nstp,iZoop)     &   ! ZOO(N): zooplankton (umol C L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,t(i,j,:,nstp,iT13C)     &   ! DI13C(N): 13C of DIC (umol kg-1)
#endif
#if defined NUTRIENTS         
     &            ,t(i,j,:,nstp,iNO3_)     &   ! NO3(N): NO3 (umol L-1)
     &            ,t(i,j,:,nstp,iNO2_)     &   ! NO2(N): NO2 (umol L-1)
     &            ,t(i,j,:,nstp,iNH4_)     &   ! NH4(N): NH4 (umol L-1)
     &            ,t(i,j,:,nstp,iPO4_)     &   ! PO4(N): PO4 (umol L-1)
# if defined ORGANIC_MATTER
     &            ,t(i,j,:,nstp,iDON_)     &   ! DON(N): Dissolved organic nitrogen (DON: umol L-1)
     &            ,t(i,j,:,nstp,iPON_)     &   ! PON(N): Particulate organic nitrogen (PON: umol L-1)
     &            ,t(i,j,:,nstp,iDOP_)     &   ! DOP(N): Dissolved organic phosporius (DOP: umol L-1)
     &            ,t(i,j,:,nstp,iPOP_)     &   ! POP(N): Particulate organic phosporius (POP: umol L-1)
# endif
#endif
#if defined COT_STARFISH         
     &            ,t(i,j,:,nstp,iCOTe)     &   ! COTe(N): COT starfish egg (umol L-1)
     &            ,t(i,j,:,nstp,iCOTl)     &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
!          output parameters
     &            ,dtrc_dt(:,iTIC_)   &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
     &            ,dtrc_dt(:,iTAlk)   &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
     &            ,dtrc_dt(:,iOxyg)   &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
#if defined ORGANIC_MATTER
     &            ,dtrc_dt(:,iDOC_)   &   ! dDOC_dt(N): dDOC/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPOC_)   &   ! dPOC_dt(N): dPOC/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPhyt)   &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
     &            ,dtrc_dt(:,iZoop)   &   ! dZOO_dt(N): dZOO/dt (umol C L-1 s-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,dtrc_dt(:,iT13C)   &   ! dDI13C_dt(N): dDI13C/dt (umol kg-1 s-1)
#endif
#if defined NUTRIENTS 
     &            ,dtrc_dt(:,iNO3_)   &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iNO2_)   &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iNH4_)   &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPO4_)   &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
# if defined ORGANIC_MATTER
     &            ,dtrc_dt(:,iDON_)   &   ! dDON_dt(N): dDON/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPON_)   &   ! dPON_dt(N): dPON/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iDOP_)   &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPOP_)   &   ! dPOP_dt(N): dPOP/dt (umol L-1 s-1)
# endif
#endif
#if defined COT_STARFISH         
     &            ,dtrc_dt(:,iCOTe)   &   ! dCOTe/dt(N): (umol L-1 s-1)
     &            ,dtrc_dt(:,iCOTl)   &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
     &            ,sspH           &   ! sea surface pH
     &            ,ssfCO2         &   ! sea surface fCO2 (uatm)
     &            ,ssWarg         &   ! sea surface aragonite saturation state
     &            ,ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
     &            ,ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
     &            ,PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
     &            ,coral_Pg       &   ! Coral gross photosynthesis rate (nmol cm-2 s-1)
     &            ,coral_R        &   ! Coral respiration rate (nmol cm-2 s-1)
     &            ,coral_Gn       &   ! Coral calcification rate (nmol cm-2 s-1)
     &            ,sgrass_Pg      &   ! seagrass gross photosynthesis rate (mmol m-2 s-1)
     &            ,sgrass_R       &   ! seagrass respiration rate (mmol m-2 s-1)
     &             )
!
#ifdef MASKING
          END IF
#endif

            HisBio2d(i,j,ipHt_) = sspH
            HisBio2d(i,j,iWarg) = ssWarg
            HisBio2d(i,j,iCOfx) = ssCO2flux
            HisBio2d(i,j,ipCO2) = ssfCO2
            HisBio2d(i,j,iO2fx) = ssO2flux
            HisBio2d(i,j,iPARb) = PFDbott
            HisBio2d(i,j,iTau_) = tau
#ifdef CORAL_POLYP
            HisBio2d(i,j,iClPg) = coral_Pg
            HisBio2d(i,j,iCl_R) = coral_R
            HisBio2d(i,j,iClPn) = coral_Pg-coral_R
            HisBio2d(i,j,iCl_G) = coral_Gn
            HisBio2d(i,j,iCogC) = CORAL(ng)%QC(1,i,j)
# ifdef CORAL_CARBON_ISOTOPE
            R13CH2O = CORAL(ng)%Q13C(1,i,j) / CORAL(ng)%QC(1,i,j)   !coral organism
            HisBio2d(i,j,iC13t) = d13C_fromR13C(R13CH2O)
# endif
# ifdef CORAL_ZOOXANTHELLAE
            HisBio2d(i,j,iCzox) = ZOOX(ng)%dens(1,i,j)
# endif
# ifdef CORAL_SIZE_DYNAMICS
            HisBio2d(i,j,iCmrt) = CORAL(ng)%mort(1,i,j)
            HisBio2d(i,j,iCgrw) = CORAL(ng)%growth(1,i,j)
# endif
#endif
#ifdef SEAGRASS
            HisBio2d(i,j,iSgPg) = sgrass_Pg
            HisBio2d(i,j,iSg_R) = sgrass_R
            HisBio2d(i,j,iSgPn) = sgrass_Pg-sgrass_R
#endif

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

          DO k=1,N(ng)
            DO itrc=1,NBT
              ibio=idbio(itrc)
              
              IF(dtrc_dt(k,ibio)*0.0_r8 /= 0.0_r8) THEN  !!!---------Error Handling: Check NAN
                dtrc_dt(k,ibio)=0.0_r8
                write(50,*) i,j,k,ibio,dtrc_dt(k,ibio)                &
    &            ,t(i,j,k,nnew,ibio),t(i,j,k,nstp,ibio)               &
    &            ,coral_Pg,coral_R,sgrass_Pg,sgrass_R,ssO2flux        &
    &            ,ssCO2flux,rmask_io(i,j)
              END IF
              
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)                    &
    &                              +dtrc_dt(k,ibio)*dt(ng)*Hz(i,j,k)
    
              t(i,j,k,nnew,ibio)=MAX(0.0_r8,t(i,j,k,nnew,ibio))!!!---------Error Handling
              
            END DO

#if defined CARBON_ISOTOPE
! Carbon isotope ratio calculation
            HisBio3d(i,j,k,id13C)=d13C_fromR13C(t(i,j,k,nnew,iT13C)/t(i,j,k,nnew,iTIC_))
#endif
          END DO

        END DO
      END DO
!-----------------------------------------------------------------------
!$acc end kernels

      RETURN
      END SUBROUTINE biology_tile
      

