!
!svn $Id: fennel_mod.h 585 2012-01-03 18:44:28Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Parameters for Fennel et al. (2006) model:                          !
!                                                                      !
!   AttSW    Light attenuation due to sea water [1/m].                 !
!   AttChl   Light attenuation by Chlorophyll [1/(mg_Chl m2)].         !
!   BioIter  Maximum number of iterations to achieve convergence       !
!              of the nonlinear solution.                              !
!   Chl2C_m  Maximum chlorophyll to carbon ratio [mg_Chl/mg_C].        !
!   ChlMin   Chlorophill minimum threshold value [mg_Chl/m3].          !
!   CoagR    Coagulation rate: agregation rate of SDeN + Phyt ==> LDeN !
!              [1/day].                                                !
!   D_p5NH4  Half-saturation radiation for nitrification inhibition    !
!              [Watts/m2].                                             !
!   I_thNH4  Radiation threshold for nitrification inhibition          !
!              [Watts/m2].                                             !
!   K_NH4    Inverse half-saturation for Phytoplankton NH4 uptake      !
!              [m3/(mmol_N)].                                          !
!   K_NO3    Inverse half-saturation for Phytoplankton NO3 uptake      !
!              [m3/(mmol_N)].                                          !
!   K_Phy    Zooplankton half-saturation, squared constant for         !
!              ingestion [mmol_N/m3]^2.                                !
!   LDeRR    Large Detrital re-mineralization rate [1/day].            !
!   NitriR   Nitrification rate: oxidation of NH4 to NO3 [1/day].      !
!   PARfrac  Fraction of shortwave radiation that is available for     !
!              photosyntesis [nondimensional].                         !
!   PhyCN    Phytoplankton Carbon:Nitrogen ratio [mol_C/mol_N].        !
!   PhyIP    Phytoplankton NH4 inhibition parameter [1/(mmol_N)].      !
!   PhyIS    Phytoplankton, initial slope of the P-I curve             !
!              [mg_C/(mg_Chl W m-2 day)].                              !
!   ZooMin   Phytoplankton minimum threshold value [mmol_N/m3].        !
!   PhyMR    Phytoplankton mortality rate [1/day] to small detritus.   !
!   SDeAR    Small detritus aggregation rate into Large detritus       !
!              [1/day].                                                !
!   SDeBR    Small Detrital breakdown to NH4 rate [1/day].             !
!   SDeRR    Large Detrital re-mineralization rate [1/day].            !
!   Vp0      Eppley temperature-limited and light-limited growth       !
!              tuning parameter [nondimensional].                      !
!   wLDet    Vertical sinking velocities for Large Detritus            !
!              fraction [m/day].                                       !
!   wPhy     Vertical sinking velocity for Phytoplankton               !
!              fraction [m/day].                                       !
!   wSDet    Vertical sinking velocities for Small Detritus            !
!              fraction [m/day].                                       !
!   ZooAE_N  Zooplankton nitrogen assimilation efficiency fraction     !
!              [nondimensional].                                       !
!   ZooBM    Zooplankton basal metabolism [1/day].                     !
!   ZooCN    Zooplankton Carbon:Nitrogen ratio [mol_C/mol_N].          !
!   ZooER    Zooplankton specific excretion rate [1/day].              !
!   ZooGR    Zooplankton maximum growth rate [1/day].                  !
!   ZooMin   Zooplankton minimum threshold value [mmol_N/m3].          !
!   ZooMR    Zooplankton mortality to Detritus [1/day].                !
!   pCO2air  CO2 partial pressure in the air [ppmv].                   !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
      integer :: idCorl                 ! Coral coverage
      integer :: idSgrs                 ! Seagrass coverage
      integer :: idAlga                 ! Algal coverage
      
      integer :: idd13C                 ! d13C of DIC

      integer, allocatable :: idbio(:)  ! Biological tracers
      integer :: iTIC_                  ! Total inorganic carbon
      integer :: iTAlk                  ! Total alkalinity
      integer :: iOxyg                  ! Dissolved oxygen concentration
#ifdef CARBON_ISOTOPE
      integer :: iT13C                  ! Corbon 13 of total inorganic carbon
#endif
#ifdef NUTRIENTS
      integer :: iNO3_                  ! Nitrate concentration
      integer :: iNO2_                  ! Nitrate concentration
      integer :: iNH4_                  ! Ammonium concentration
      integer :: iPO4_                  ! Ammonium concentration
      integer :: iChlo                  ! Chlorophyll concentration
      integer :: iPhyt                  ! Phytoplankton concentration
      integer :: iZoop                  ! Zooplankton concentration
      integer :: iLDeC                  ! Large detritus C-concentration
      integer :: iSDeC                  ! Small detritus C-concentration
      integer :: iLDeN                  ! Large detritus N-concentration
      integer :: iSDeN                  ! Small detritus N-concentration
      integer :: iLDeP                  ! Large detritus P-concentration
      integer :: iSDeP                  ! Small detritus P-concentration
#endif
!
!  Biological 2D Histrory variable IDs.
!
      integer, allocatable :: iHbio2(:)       ! 2D biological terms
      integer  :: iClPg                       ! coral gross photosynthesis rate
      integer  :: iCl_R                       ! coral respiration rate
      integer  :: iClPn                       ! coral net photosynthesis rate
      integer  :: iCl_G                       ! coral calcification rate
      integer  :: iCogC                       ! coral tissue organic carbon
#ifdef CORAL_CARBON_ISOTOPE
      integer  :: iC13t                       ! coral tissue carbon isotope ratio
#endif
#ifdef CORAL_ZOOXANTHELLAE
      integer  :: iCzox                       ! coral zooxanthellae density
#endif
      integer  :: iSgPg                       ! seagrass gross photosynthesis rate
      integer  :: iSg_R                       ! seagrass respiration rate
      integer  :: iSgPn                       ! seagrass net photosynthesis rate
      integer  :: ipHt_                       ! sea surface pH (total scale)
      integer  :: iWarg                       ! sea surface aragonite saturation state

      integer  :: iCOfx                       ! air-sea CO2 flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
      integer  :: iPARb                       ! bottom photon flux density (umol m-2 s-1)
#ifdef NUTRIENTS
      integer  :: iDNIT                       ! denitrification flux
#endif
      integer, allocatable :: iHbio3(:)       ! 3D biological terms
      integer  :: iPPro                       ! primary productivity
#ifdef CARBON_ISOTOPE
      integer  :: id13C                  ! d13C of total inorganic carbon
#endif
#ifdef NUTRIENTS
      integer  :: iNO3u                       ! NO3 uptake
#endif

      integer, allocatable :: iDbio2(:)       ! 2D biological terms
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)

      real(r8), allocatable :: AttSW(:)              ! 1/m
      real(r8), allocatable :: AttChl(:)             ! 1/(mg_Chl m2)
      real(r8), allocatable :: Chl2C_m(:)            ! mg_Chl/mg_C
      real(r8), allocatable :: ChlMin(:)             ! mg_Chl/m3
      real(r8), allocatable :: CoagR(:)              ! 1/day
      real(r8), allocatable :: D_p5NH4(:)            ! Watts/m2
      real(r8), allocatable :: I_thNH4(:)            ! Watts/m2
      real(r8), allocatable :: K_NH4(:)              ! m3/mmol_N
      real(r8), allocatable :: K_NO3(:)              ! m3/mmol_N
      real(r8), allocatable :: K_Phy(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: LDeRRN(:)             ! 1/day
      real(r8), allocatable :: LDeRRC(:)             ! 1/day
      real(r8), allocatable :: NitriR(:)             ! 1/day
      real(r8), allocatable :: PARfrac(:)            ! nondimensional
      real(r8), allocatable :: PhyCN(:)              ! mol_C/mol_N
      real(r8), allocatable :: PhyIP(:)              ! 1/mmol_N
      real(r8), allocatable :: PhyIS(:)              ! 1/(Watts m-2 day)
      real(r8), allocatable :: PhyMin(:)             ! mmol_N/m3
      real(r8), allocatable :: PhyMR(:)              ! 1/day
      real(r8), allocatable :: SDeAR(:)              ! 1/day
      real(r8), allocatable :: SDeBR(:)              ! 1/day
      real(r8), allocatable :: SDeRRN(:)             ! 1/day
      real(r8), allocatable :: SDeRRC(:)             ! 1/day
      real(r8), allocatable :: Vp0(:)                ! nondimensional
      real(r8), allocatable :: wLDet(:)              ! m/day
      real(r8), allocatable :: wPhy(:)               ! m/day
      real(r8), allocatable :: wSDet(:)              ! m/day
      real(r8), allocatable :: ZooAE_N(:)            ! nondimensional
      real(r8), allocatable :: ZooBM(:)              ! 1/day
      real(r8), allocatable :: ZooCN(:)              ! mol_C/mol_N
      real(r8), allocatable :: ZooER(:)              ! 1/day
      real(r8), allocatable :: ZooGR(:)              ! 1/day
      real(r8), allocatable :: ZooMin(:)             ! mmol_N/m3
      real(r8), allocatable :: ZooMR(:)              ! 1/day
      real(r8), allocatable :: pCO2air(:)            ! ppmv
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
      real(r8), allocatable :: TAlk0(:)              ! umol/kg
      real(r8), allocatable :: TIC_0(:)              ! umol/kg
      real(r8), allocatable :: Oxyg0(:)              ! umol/L
      real(r8), allocatable :: d13C0(:)              ! per mill
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================

!
!  Local variable declarations
!
      integer :: i, ic

!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS

      i=1
      iTIC_=ic+i
      i=i+1
      iTAlk=ic+i
      i=i+1
      iOxyg=ic+i  !  4
#ifdef CARBON_ISOTOPE
      i=i+1
      iT13C=ic+i  ! +1
#endif
#ifdef NUTRIENTS
      i=i+1
      iNO3_=ic+i
      i=i+1
      iNO2_=ic+i
      i=i+1
      iNH4_=ic+i
      i=i+1
      iPO4_=ic+i
      i=i+1
      iChlo=ic+i
      i=i+1
      iPhyt=ic+i
      i=i+1
      iZoop=ic+i
      i=i+1
      iLDeC=ic+i
      i=i+1
      iSDeC=ic+i
      i=i+1
      iLDeN=ic+i
      i=i+1
      iSDeN=ic+i
      i=i+1
      iLDeP=ic+i
      i=i+1
      iSDeP=ic+i ! +13
#endif

!-----------------------------------------------------------------------
!  Determine number of biological tracers.
!-----------------------------------------------------------------------

      NBT=i
!---------------------------------------------------------------------
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
      END IF

      DO i=1,NBT
        idbio(i)=NAT+NPT+NCS+NNS+i
      END DO

!
!-----------------------------------------------------------------------
!  Set sources and sinks biology history/diagnostic parameters.
!-----------------------------------------------------------------------

!
!  Initialize 2D biology indices.
!
      ic=1     ! ic reset
      ipHt_=ic
      ic=ic+1
      iWarg=ic

      ic=ic+1
      iCOfx=ic
      ic=ic+1
      ipCO2=ic
      ic=ic+1
      iO2fx=ic

      ic=ic+1
      iPARb=ic

      ic=ic+1
      iClPg=ic
      ic=ic+1
      iCl_R=ic
      ic=ic+1
      iClPn=ic
      ic=ic+1
      iCl_G=ic
      ic=ic+1
      iCogC=ic
#ifdef CORAL_CARBON_ISOTOPE
      ic=ic+1
      iC13t=ic
#endif
#ifdef CORAL_ZOOXANTHELLAE
      ic=ic+1
      iCzox=ic
#endif
      ic=ic+1
      iSgPg=ic
      ic=ic+1
      iSg_R=ic
      ic=ic+1
      iSgPn=ic

#ifdef NUTRIENTS
      ic=ic+1
      iDNIT=ic
#endif
!  Set number of 2D history terms.
!
      NHbio2d=ic
!
!  Allocate biological history vectors
!
      IF (.not.allocated(iHbio2)) THEN
        allocate ( iHbio2(NHbio2d) )
      END IF

!----------------------------------------------------------------------
!  Initialize 3D biology indices.
!
      ic=1     ! ic reset
      iPPro=ic
#ifdef CARBON_ISOTOPE
      ic=ic+1
      id13C=ic  ! +1
#endif
#ifdef NUTRIENTS
      ic=ic+1
      iNO3u=ic
#endif
!
!  Set number of 3D history terms.
!
      NHbio3d=ic


      IF (.not.allocated(iHbio3)) THEN
        allocate ( iHbio3(NHbio3d) )
      END IF

!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
      END IF
      IF (.not.allocated(AttSW)) THEN
        allocate ( AttSW(Ngrids) )
      END IF
      IF (.not.allocated(AttChl)) THEN
        allocate ( AttChl(Ngrids) )
      END IF
      IF (.not.allocated(Chl2C_m)) THEN
        allocate ( Chl2C_m(Ngrids) )
      END IF
      IF (.not.allocated(ChlMin)) THEN
        allocate ( ChlMin(Ngrids) )
      END IF
      IF (.not.allocated(CoagR)) THEN
        allocate ( CoagR(Ngrids) )
      END IF
      IF (.not.allocated(D_p5NH4)) THEN
        allocate ( D_p5NH4(Ngrids) )
      END IF
      IF (.not.allocated(I_thNH4)) THEN
        allocate ( I_thNH4(Ngrids) )
      END IF
      IF (.not.allocated(K_NH4)) THEN
        allocate ( K_NH4(Ngrids) )
      END IF
      IF (.not.allocated(K_NO3)) THEN
        allocate ( K_NO3(Ngrids) )
      END IF
      IF (.not.allocated(K_Phy)) THEN
        allocate ( K_Phy(Ngrids) )
      END IF
      IF (.not.allocated(LDeRRN)) THEN
        allocate ( LDeRRN(Ngrids) )
      END IF
      IF (.not.allocated(LDeRRC)) THEN
        allocate ( LDeRRC(Ngrids) )
      END IF
      IF (.not.allocated(NitriR)) THEN
        allocate ( NitriR(Ngrids) )
      END IF
      IF (.not.allocated(PARfrac)) THEN
        allocate ( PARfrac(Ngrids) )
      END IF
      IF (.not.allocated(PhyCN)) THEN
        allocate ( PhyCN(Ngrids) )
      END IF
      IF (.not.allocated(PhyIP)) THEN
        allocate ( PhyIP(Ngrids) )
      END IF
      IF (.not.allocated(PhyIS)) THEN
        allocate ( PhyIS(Ngrids) )
      END IF
      IF (.not.allocated(PhyMin)) THEN
        allocate ( PhyMin(Ngrids) )
      END IF
      IF (.not.allocated(PhyMR)) THEN
        allocate ( PhyMR(Ngrids) )
      END IF
      IF (.not.allocated(SDeAR)) THEN
        allocate ( SDeAR(Ngrids) )
      END IF
      IF (.not.allocated(SDeBR)) THEN
        allocate ( SDeBR(Ngrids) )
      END IF
      IF (.not.allocated(SDeRRN)) THEN
        allocate ( SDeRRN(Ngrids) )
      END IF
      IF (.not.allocated(SDeRRC)) THEN
        allocate ( SDeRRC(Ngrids) )
      END IF
      IF (.not.allocated(Vp0)) THEN
        allocate ( Vp0(Ngrids) )
      END IF
      IF (.not.allocated(wLDet)) THEN
        allocate ( wLDet(Ngrids) )
      END IF
      IF (.not.allocated(wPhy)) THEN
        allocate ( wPhy(Ngrids) )
      END IF
      IF (.not.allocated(wSDet)) THEN
        allocate ( wSDet(Ngrids) )
      END IF
      IF (.not.allocated(ZooAE_N)) THEN
        allocate ( ZooAE_N(Ngrids) )
      END IF
      IF (.not.allocated(ZooBM)) THEN
        allocate ( ZooBM(Ngrids) )
      END IF
      IF (.not.allocated(ZooCN)) THEN
        allocate ( ZooCN(Ngrids) )
      END IF
      IF (.not.allocated(ZooER)) THEN
        allocate ( ZooER(Ngrids) )
      END IF
      IF (.not.allocated(ZooGR)) THEN
        allocate ( ZooGR(Ngrids) )
      END IF
      IF (.not.allocated(ZooMin)) THEN
        allocate ( ZooMin(Ngrids) )
      END IF
      IF (.not.allocated(ZooMR)) THEN
        allocate ( ZooMR(Ngrids) )
      END IF
      IF (.not.allocated(pCO2air)) THEN
        allocate ( pCO2air(Ngrids) )
      END IF
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
      IF (.not.allocated(TAlk0)) THEN
        allocate ( TAlk0(Ngrids) )
      END IF
      IF (.not.allocated(TIC_0)) THEN
        allocate ( TIC_0(Ngrids) )
      END IF
      IF (.not.allocated(Oxyg0)) THEN
        allocate ( Oxyg0(Ngrids) )
      END IF
      IF (.not.allocated(d13C0)) THEN
        allocate ( d13C0(Ngrids) )
      END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
      RETURN
      END SUBROUTINE initialize_biology
