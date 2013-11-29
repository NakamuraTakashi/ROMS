
!!!=== ver 2013/11/29   Copyright (c) 2012-2013 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!**** MODULE OF ECOSYSTEM MODEL ************************************

  MODULE mod_reef_ecosys

  contains

!!! **********************************************************************
!!!  Set initial conditions for reef ecosystem model
!!! **********************************************************************

    SUBROUTINE initialize_reef_ecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
      
      USE mod_coral
      USE mod_seagrass
      
      implicit none

      integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

      CALL initialize_coral(ng, Ngrids, LBi, UBi, LBj, UBj)
      CALL initialize_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)
      
      RETURN
    END SUBROUTINE initialize_reef_ecosys


!!! **********************************************************************
!!!  Main program of reef ecosystem model
!!! **********************************************************************

    SUBROUTINE reef_ecosys        &
!          input parameters
     &            (ng, i, j       &   ! ng: nested grid number; i,j: position
     &            ,N              &   ! Number of vertical grid (following ROMS vertical grid)
     &            ,isplit         &   ! Internal loop counts of coral polyp model
     &            ,dt             &   ! Time step (sec)
     &            ,dz             &   ! dz(N): vertical grid size (m)
     &            ,PFDsurf        &   ! Sea surface photon flux density (umol m-2 s-1)
     &            ,tau            &   ! bottom shear stress (N m-2)
     &            ,pCO2air        &   ! Air CO2 pertial pressure (uatm)
     &            ,U10            &   ! wind speed (m s-1)

     &            ,p_coral        &   ! Coral coverage (0-1)
     &            ,p_sgrass       &   ! seagrass coverage (0-1)
     &            ,p_algae        &   ! algal coverage (0-1)

     &            ,Tmp            &   ! Tmp(N): Temperature (oC)
     &            ,Sal            &   ! Sal(N): Salinity (PSU)
     &            ,DIC            &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TA             &   ! TA (N): Total alkalinity (TA: umol kg-1)
     &            ,DOx            &   ! DOx(N): Dissolved oxygen (umol L-1)
#if defined NUTRIENTS            
     &            ,NO3            &   ! NO3(N): NO3 (umol L-1)
     &            ,NO2            &   ! NO2(N): NO2 (umol L-1)
     &            ,NH4            &   ! NH4(N): NH4 (umol L-1)
     &            ,PO4            &   ! PO4(N): PO4 (umol L-1)
     &            ,DOC            &   ! DOC(N): Dissolved organic carbon (DOC: umol L-1)
     &            ,DON            &   ! DON(N): Dissolved organic nitrogen (DON: umol L-1)
     &            ,DOP            &   ! DOP(N): Dissolved organic phosporius (DOP: umol L-1)
     &            ,PHY            &   ! PHY(N): phytoplankton (umol C L-1)
     &            ,ZOO            &   ! ZOO(N): zooplankton (umol C L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13C          &   ! DI13C(N): 13C of DIC (umol kg-1)
#endif

!          output parameters
     &            ,dDIC_dt        &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
     &            ,dTA_dt         &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
     &            ,dDOx_dt        &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
#if defined NUTRIENTS          
     &            ,dNO3_dt        &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
     &            ,dNO2_dt        &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
     &            ,dNH4_dt        &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
     &            ,dPO4_dt        &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
     &            ,dDOC_dt        &   ! dDOC_dt(N): dDOC/dt (umol L-1 s-1)
     &            ,dDON_dt        &   ! dDON_dt(N): dDON/dt (umol L-1 s-1)
     &            ,dDOP_dt        &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
     &            ,dPHY_dt        &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
     &            ,dZOO_dt        &   ! dZOO_dt(N): dZOO/dt (umol C L-1 s-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,dDI13C_dt      &   ! dDI13C_dt(N): dDI13C/dt (umol kg-1 s-1)
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
!-----------------------------------------------------------------------
!                                                                       
!                     rho point    Face                                 
!                       (i,j)                                           
!                    _____|______  _N    Surface                        
!                   /     |      /|                                     
!      z     Layer /___________ / |                                     
!                  |           |  |_N-1                                 
!     dz(N) {   N  |           | /|                                     
!                  |___________|/ : :                                   
!                  |           |  :      Water column                   
!               :  :           :  |_2                                   
!               :  :           : /|                                     
!               :  |___________|/ |                                     
!                  |           |  |_1                                   
!     dz(2) {   2  |           | /|                                     
!                  |___________|/ |                                     
!                  |           |  |_0    Bottom                         
!     dz(1) {   1  |           | /                                      
!                  |___________|/                                       
!                                                                       
!                                                                       
!      A vertical section of the ecosys grid showing water column.      
!-----------------------------------------------------------------------

      USE mod_coral
      USE mod_seagrass
      USE mod_geochem
      
      implicit none

! input parameters
      integer, intent(in) :: ng,i,j     
      integer, intent(in) :: N          
      integer, intent(in) :: isplit     
      real(8), intent(in) :: dt         
      real(8), intent(in) :: dz(N)      
      real(8), intent(in) :: PFDsurf    
      real(8), intent(in) :: tau        
      real(8), intent(in) :: pCO2air    
      real(8), intent(in) :: U10        

      real(8), intent(in) :: p_coral    
      real(8), intent(in) :: p_sgrass   
      real(8), intent(in) :: p_algae    

      real(8), intent(in) :: Tmp(N)        
      real(8), intent(in) :: Sal(N)        
      real(8), intent(in) :: DIC(N)        
      real(8), intent(in) :: TA (N)        
      real(8), intent(in) :: DOx(N)        
#if defined NUTRIENTS            
      real(8), intent(in) :: NO3(N)        
      real(8), intent(in) :: NO2(N)        
      real(8), intent(in) :: NH4(N)        
      real(8), intent(in) :: PO4(N)        
      real(8), intent(in) :: DOC(N)        
      real(8), intent(in) :: DON(N)        
      real(8), intent(in) :: DOP(N)        
      real(8), intent(in) :: PHY(N)        
      real(8), intent(in) :: ZOO(N)        
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(in) :: DI13C(N)      
#endif
! output parameters
      real(8), intent(out) :: dDIC_dt(N)     
      real(8), intent(out) :: dTA_dt(N)     
      real(8), intent(out) :: dDOx_dt(N)     
#if defined NUTRIENTS           
      real(8), intent(out) :: dNO3_dt(N)     
      real(8), intent(out) :: dNO2_dt(N)     
      real(8), intent(out) :: dNH4_dt(N)     
      real(8), intent(out) :: dPO4_dt(N)     
      real(8), intent(out) :: dDOC_dt(N)     
      real(8), intent(out) :: dDON_dt(N)     
      real(8), intent(out) :: dDOP_dt(N)     
      real(8), intent(out) :: dPHY_dt(N)     
      real(8), intent(out) :: dZOO_dt(N)     
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(out) :: dDI13C_dt(N)   
#endif
      real(8), intent(out) :: sspH      
      real(8), intent(out) :: ssfCO2    
      real(8), intent(out) :: ssWarg    
      real(8), intent(out) :: ssCO2flux 
      real(8), intent(out) :: ssO2flux  
      real(8), intent(out) :: PFDbott   
      real(8), intent(out) :: coral_Pg  
      real(8), intent(out) :: coral_R   
      real(8), intent(out) :: coral_Gn  
      real(8), intent(out) :: sgrass_Pg 
      real(8), intent(out) :: sgrass_R  

      real(8), parameter :: AttSW  = 0.12d0       ! Light attenuation due to seawater [1/m], {0.04d0}.
      real(8), parameter :: AttChl = 0.02486d0    ! Light attenuation by chlorophyll [1/(mg_Chl m2)], {0.02486d0}.
      
      real(8) :: dtc                              ! Internal time step for coral polyp model (sec)

      integer :: k, m          
      real(8) :: TmpK
      real(8) :: DOsatu    
      real(8) :: cCO2aq, cHCO3, cCO3, R13C
      real(8) :: PFD, Att, AttFac, ExpAtt, Itop, cff

      real(8) :: DICuptake, TAuptake,  DOuptake
      real(8) :: NO3uptake, NO2uptake, NH4uptake, PO4uptake  
      real(8) :: DOCuptake, DONuptake, DOPuptake    
      real(8) :: PHYuptake, ZOOuptake  
      real(8) :: DI13Cuptake


!-----------------------------------------------------------------------
! Initialize all time difference values (d_XXX)
!-----------------------------------------------------------------------
      
      dtc = dt/isplit                  ! Internal time step for coral polyp model (sec)
      
      DO k=1,N
        dDIC_dt(k) = 0.0d0
        dTA_dt(k)  = 0.0d0
        dDOx_dt(k) = 0.0d0
#if defined NUTRIENTS            
        dNO3_dt(k) = 0.0d0
        dNO2_dt(k) = 0.0d0
        dNH4_dt(k) = 0.0d0
        dPO4_dt(k) = 0.0d0
        dDOC_dt(k) = 0.0d0
        dDON_dt(k) = 0.0d0
        dDOP_dt(k) = 0.0d0
        dPHY_dt(k) = 0.0d0
        dZOO_dt(k) = 0.0d0
#endif
#if defined CARBON_ISOTOPE
        dDI13C_dt(k) = 0.0d0
#endif
      END DO
      sspH      = 8.0d0   ! sea surface pH
      ssfCO2    = 380.0d0   ! sea surface fCO2 (uatm)
      ssWarg    = 4.0d0   ! sea surface aragonite saturation state
      ssCO2flux = 0.0d0   ! sea surface CO2 flux (mmol m-2 s-1)
      ssO2flux  = 0.0d0   ! sea surface O2 flux (mmol m-2 s-1)
      PFDbott   = 0.0d0   ! Bottom photon flux density (umol m-2 s-1)
      coral_Pg  = 0.0d0   ! Coral gross photosynthesis rate (nmol cm-2 s-1)
      coral_R   = 0.0d0   ! Coral respiration rate (nmol cm-2 s-1)
      coral_Gn  = 0.0d0   ! Coral calcification rate (nmol cm-2 s-1)
      sgrass_Pg = 0.0d0   ! seagrass gross photosynthesis rate (mmol m-2 s-1)
      sgrass_R  = 0.0d0   ! seagrass respiration rate (mmol m-2 s-1)


!!!---------------------------------------------------------------------
!!!  Sea surface interaction.
!!!---------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Compute surface O2 gas exchange.
!-----------------------------------------------------------------------

      TmpK = tmp(N)+273.15d0

!----------- O2 gas exchange rate (mmol m-2 s-1). -------------------
      
      DOsatu=O2satu(TmpK,Sal(N))
      ssO2flux = Flux_O2(DOx(N), DOsatu, U10, TmpK, Sal(N) )  ! sea to air is positive

!-----------------------------------------------------------------------
!  Compute surface CO2 gas exchange.
!-----------------------------------------------------------------------

!----------- CO2 system in ambient seawater -------------------

      sspH = pH_fromATCT( TA(N), DIC(N),TmpK, Sal(N) )
      cCO2aq = cCO2aq_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
!      cHCO3 = cHCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
      cCO3 = cCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
      ssfCO2 = fCO2_fromcCO2aq( cCO2aq, TmpK, Sal(N) )  !! for output
      ssWarg = Warg_fromcCO3( cCO3, TmpK, Sal(N) )

!----------- CO2 gas exchange rate (mmol m-2 s-1). -------------------
      ssCO2flux = Flux_CO2(ssfCO2, pCO2air, U10, TmpK, Sal(N) )  ! sea to air is positive 

#if defined AIR_SEA_GAS_EXCHANGE
      dDOx_dt(N) = dDOx_dt(N) - ssO2flux  /dz(N)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      dDIC_dt(N) = dDIC_dt(N) - ssCO2flux  /dz(N)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# if defined CARBON_ISOTOPE
      R13C=R13C_fromd13C(-10.d0)
      dDI13C_dt(N) = dDI13C_dt(N)- ssCO2flux*R13C /dz(N) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# endif
#endif


!!!---------------------------------------------------------------------
!!!  Water columun interactions.
!!!---------------------------------------------------------------------

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
      PFD=PFDsurf

      DO k=N,1,-1

!!
!!  Compute average light attenuation for each grid cell. To include
!!  other attenuation contributions like suspended sediment or CDOM
!!  modify AttFac.
!        AttFac=0.0d0
!
!        Att=(AttSW+                                         &
!     &       AttChl*Bio(i,k,iChlo)+                         &
!     &       AttFac)*dz(k)
!        ExpAtt=EXP(-Att)
!        Itop=PFD
!        PFD=Itop*(1.0d0-ExpAtt)/Att    ! average at cell center
!!
!!  Compute Chlorophyll-a phytoplankton ratio, [mg Chla / (mg C)].
!!
!        cff=PhyCN(ng)*12.0d0
!        Chl2C=MIN(Bio(i,k,iChlo)/(Bio(i,k,iPhyt)*cff+eps),      &
!     &            Chl2C_m(ng))
!
        Att= AttSW*dz(k) 
        ExpAtt=EXP(-Att)
        Itop=PFD
        PFD=Itop*(1.0d0-ExpAtt)/Att    ! average at cell center

!-----------------------------------------------------------------------
! Food web model.
!-----------------------------------------------------------------------


!!! Add food web reaction !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!  Light attenuation at the bottom of the grid cell. It is the starting
!  PFD value for the next (deeper) vertical grid cell.
!
        PFD=Itop*ExpAtt

      END DO

        PFDbott=PFD

!!!---------------------------------------------------------------------
!!!  Benthic interactions.
!!!---------------------------------------------------------------------

#ifdef CORAL_POLYP

!-----------------------------------------------------------------------
! Compute coral polyp model.
!-----------------------------------------------------------------------
!
      IF(p_coral .gt. 0.0d0) THEN          

        DO m=1,isplit   !!! Loop for coral polyp model: dtc <= 0.05 sec

          CALL coral_polyp         &
!          input parameters
     &            (ng, 1, i, j     &   ! ng: nested grid number; n: coral compartment; i,j: position
     &            ,dtc             &   ! Time step (sec)
     &            ,PFDbott         &   ! Photon flux density (umol m-2 s-1)
     &            ,Tmp(1)          &   ! Temperature (oC)
     &            ,Sal(1)          &   ! Salinity (PSU)
     &            ,DIC(1)          &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TA (1)          &   ! Total alkalinity (TA: umol kg-1)
     &            ,DOx(1)          &   ! Dissolved oxygen (umol L-1)
# if defined CORAL_NUTRIENTS
     &            ,NO3(1)          &   ! NO3 (umol L-1)
     &            ,NO2(1)          &   ! NO2 (umol L-1)
     &            ,NH4(1)          &   ! NH4 (umol L-1)
     &            ,PO4(1)          &   ! PO4 (umol L-1)
     &            ,DOC(1)          &   ! Dissolved organic carbon (DOC: umol L-1)
     &            ,DON(1)          &   ! Dissolved organic nitrogen (DON: umol L-1)
     &            ,DOP(1)          &   ! Dissolved organic phosporius (DOP: umol L-1)
     &            ,PHY(1)          &   ! phytoplankton (umol C L-1)
     &            ,ZOO(1)          &   ! zooplankton (umol C L-1)
# endif
# if defined CORAL_CARBON_ISOTOPE
     &            ,DI13C(1)        &   !13C of DIC (umol kg-1)
# endif
     &            ,tau             &   ! bottom shear stress (N m-2)
     &            ,0.0d0           &   ! sedimentation rate (??)
!          output parameters
     &            ,coral_Pg        &   ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,coral_R         &   ! Respiration rate (nmol cm-2 s-1)
     &            ,coral_Gn        &   ! Galcification rate (nmol cm-2 s-1)
     &            ,DICuptake       &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,TAuptake        &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake        &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# if defined CORAL_NUTRIENTS
     &            ,NO3uptake       &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NO2uptake       &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake       &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake       &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,DOCuptake       &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,DONuptake       &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,DOPuptake       &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,PHYuptake       &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,ZOOuptake       &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CORAL_CARBON_ISOTOPE
     &            ,DI13Cuptake     &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
     &             )

        END DO

      ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convaert [nmol cm-2 s-1] to [umol L-1]

        cff=0.01d0 /dz(1) * p_coral*10.0d0  !!!*40.0d0 ‚Ä‚«‚Æ‚¤ convert projected area to coral surface area
         
        dDIC_dt(1) = dDIC_dt(1) - DICuptake * cff/1.024d0
        dTA_dt (1) = dTA_dt (1) - TAuptake  * cff/1.024d0
        dDOx_dt(1) = dDOx_dt(1) - DOuptake  * cff
# if defined NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - NO3uptake * cff
        dNO2_dt(1) = dNO2_dt(1) - NO2uptake * cff
        dNH4_dt(1) = dNH4_dt(1) - NH4uptake * cff
        dPO4_dt(1) = dPO4_dt(1) - PO4uptake * cff
        dDOC_dt(1) = dDOC_dt(1) - DOCuptake * cff
        dDON_dt(1) = dDON_dt(1) - DONuptake * cff
        dDOP_dt(1) = dDOP_dt(1) - DOPuptake * cff
        dPHY_dt(1) = dPHY_dt(1) - PHYuptake * cff
        dZOO_dt(1) = dZOO_dt(1) - ZOOuptake * cff
# endif
# if defined CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - DI13Cuptake * cff/1.024d0
# endif
      END IF
#endif

      
#ifdef SEAGRASS
      
!-----------------------------------------------------------------------
! Compute seagrass model.
!-----------------------------------------------------------------------
!
      IF(p_sgrass .gt. 0.0d0) THEN

        CALL seagrass             &
!          input parameters
     &            (ng, 1, i, j     &   ! ng: nested grid number; n: seagrass compartment; i,j: position
     &            ,PFDbott         &   ! Photon flux density (umol m-2 s-1)
     &            ,DIC(1)         &   ! DIC (umol kg-1)
     &            ,DOx(1)         &   ! DO  (umol L-1)
# if defined NUTRIENTS         
     &            ,NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
     &            ,DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!          output parameters
     &            ,sgrass_Pg       &   ! Gross photosynthesis rate (mmol m-2 s-1)
     &            ,sgrass_R        &   ! Respiration rate (mmol m-2 s-1)
     &            ,DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
     &            ,NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
     &            ,DI13Cuptake    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
     &             )

        ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convaert [nmol cm-2 s-1] to [umol L-1]

        cff = 1.0d0/dz(1) * p_sgrass

        dDIC_dt(1) = dDIC_dt(1) - DICuptake * cff/1.024d0
        dDOx_dt(1) = dDOx_dt(1) - DOuptake  * cff
# if defined NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - NO3uptake * cff
        dNH4_dt(1) = dNH4_dt(1) - NH4uptake * cff
        dPO4_dt(1) = dPO4_dt(1) - PO4uptake * cff
# endif
# if defined CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - DI13Cuptake * cff/1.024d0
# endif

      END IF
#endif

#ifdef ALGAE

!-----------------------------------------------------------------------
! Compute algae model.
!-----------------------------------------------------------------------
!
      IF(p_algae .gt. 0.0d0) THEN
       ! CALL algae model
      END IF            
#endif

      RETURN
    END SUBROUTINE reef_ecosys

!-----------------------------------------------------------------------
  END MODULE mod_reef_ecosys

