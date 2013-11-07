
!!!====== ver 2013/01/09 ===============================================

#include "cppdefs.h"


!!!**** MODULE OF CORAL POLYP MODEL ************************************

  module mod_coral

    implicit none

    integer, parameter :: nCL = 1    !! Number of coral communities

    real(8), allocatable, save :: TAcal(:,:,:) 
    real(8), allocatable, save :: TAcoe(:,:,:) 
    real(8), allocatable, save :: DICcal(:,:,:)
    real(8), allocatable, save :: DICcoe(:,:,:)
    real(8), allocatable, save :: DOcoe(:,:,:) 
    real(8), allocatable, save :: CH2O(:,:,:)

    real(8), allocatable, save :: cCO2aqcal(:,:,:)
    real(8), allocatable, save :: cCO2aqcoe(:,:,:)


#if defined CARBON_ISOTOPE
!  13C isotope
    real(8), allocatable, save :: DI13Ccal(:,:,:)
    real(8), allocatable, save :: DI13Ccoe(:,:,:)
    real(8), allocatable, save :: c13CH2O(:,:,:)
    real(8), allocatable, save :: Q13Ccoral(:,:,:)
    real(8), allocatable, save :: c13CO2aqcal(:,:,:)
    real(8), allocatable, save :: c13CO2aqcoe(:,:,:)
#endif

#if defined NUTRIENTS
!  Nutrients dynamics
    real(8), allocatable, save :: NO3coral(:,:,:)
    real(8), allocatable, save :: NO2coral(:,:,:)
    real(8), allocatable, save :: NH4coral(:,:,:)
    real(8), allocatable, save :: PO4coral(:,:,:)
    real(8), allocatable, save :: QCcoral(:,:,:)
    real(8), allocatable, save :: QNcoral(:,:,:)
    real(8), allocatable, save :: QPcoral(:,:,:)
    real(8), allocatable, save :: ROS(:,:,:)

!  zooxanthellae dynamics
    real(8), allocatable, save :: DENSzoox(:,:,:)
    real(8), allocatable, save :: QCzoox(:,:,:)
    real(8), allocatable, save :: QNzoox(:,:,:)
    real(8), allocatable, save :: QPzoox(:,:,:)
    real(8), allocatable, save :: NO3zoox(:,:,:)
    real(8), allocatable, save :: NO2zoox(:,:,:)
    real(8), allocatable, save :: NH4zoox(:,:,:)
    real(8), allocatable, save :: PO4zoox(:,:,:)

!    real(8), allocatable, save :: g_coral(:,:,:)
!    real(8), allocatable, save :: m_coral(:,:,:)
    real(8), allocatable, save :: dsed_coral(:,:,:)
#endif

  contains


!!! **********************************************************************
!!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

    subroutine initialize_coral(LBi, UBi, LBj, UBj)

      use mod_geochem
      
      implicit none
! input parameters
      integer, intent(in) :: LBi, UBi, LBj, UBj
      real(8)  R13C
      integer i,j,n


      allocate( TAcal(nCL,LBi:UBi,LBj:UBj)  ,  &
     &          TAcoe(nCL,LBi:UBi,LBj:UBj)  ,  &
     &          DICcal(nCL,LBi:UBi,LBj:UBj) ,  &
     &          DICcoe(nCL,LBi:UBi,LBj:UBj) ,  &
     &          DOcoe(nCL,LBi:UBi,LBj:UBj)  ,  &
     &          CH2O(nCL,LBi:UBi,LBj:UBj)       )

      allocate( cCO2aqcal(nCL,LBi:UBi,LBj:UBj) , &
     &          cCO2aqcoe(nCL,LBi:UBi,LBj:UBj)   )
     
#if defined CARBON_ISOTOPE
      allocate( DI13Ccal(nCL,LBi:UBi,LBj:UBj) , &
     &          DI13Ccoe(nCL,LBi:UBi,LBj:UBj) , &
     &          c13CH2O (nCL,LBi:UBi,LBj:UBj) , &
     &          Q13Ccoral(nCL,LBi:UBi,LBj:UBj)   )
      allocate( c13CO2aqcal(nCL,LBi:UBi,LBj:UBj) , &
     &          c13CO2aqcoe(nCL,LBi:UBi,LBj:UBj)  )
#endif

#if defined NUTRIENTS
      allocate( NO3coral(nCL,LBi:UBi,LBj:UBj), &
     &          NO2coral(nCL,LBi:UBi,LBj:UBj), &
     &          NH4coral(nCL,LBi:UBi,LBj:UBj), &
     &          PO4coral(nCL,LBi:UBi,LBj:UBj), &
     &          QCcoral(nCL,LBi:UBi,LBj:UBj) , &
     &          QNcoral(nCL,LBi:UBi,LBj:UBj) , &
     &          QPcoral(nCL,LBi:UBi,LBj:UBj) , &
     &          ROS(nCL,LBi:UBi,LBj:UBj)        )
     
      allocate( DENSzoox(nCL,LBi:UBi,LBj:UBj), &
     &          NO3zoox(nCL,LBi:UBi,LBj:UBj) , &
     &          NO2zoox(nCL,LBi:UBi,LBj:UBj) , &
     &          NH4zoox(nCL,LBi:UBi,LBj:UBj) , &
     &          PO4zoox(nCL,LBi:UBi,LBj:UBj) , &
     &          QCzoox(nCL,LBi:UBi,LBj:UBj)  , &
     &          QNzoox(nCL,LBi:UBi,LBj:UBj)  , &
     &          QPzoox(nCL,LBi:UBi,LBj:UBj)     )

#endif

!----------set data -----------------------

!!   Coral minimum quota
!      data QC0coral /286.d0  /   ! Minimum quota (umol cm-2)   (Tanaka et al. 2010)  ＊minimumかどうかは分からない。
!      data QN0coral / 28.d0  /   ! Minimum quota (umol cm-2)  
!      data QP0coral /  0.85d0/   ! Minimum quota (umol cm-2) 
!!
!!   Zooxanthellae minimum cell quota      
!      data QC0zoox   /8.d0 / !(pmolC/cell)  100 pg cell-1 ~ 10 pmol cell-1 = (Verde & McCloskey 1998)
!      data QN0zoox   /1.1d0/ !(pmolN/cell)  calculated from C:N:P = 116:16:1
!      data QP0zoox   /0.07d0/ !(pmolP/cell) 
!      
!      data V_zoox    / 5.d-10/ ! Volume of zooxanthellae (cm3/cells)
!                               ! ~500 um3 =5.e-10 cm3 (e.g. Jone 1997; Jones and Yellowlees 1997)
!------------------------------------------
!  Set initial conditions
      do j=LBj,UBj
        do i=LBi,UBi
          do n=1,nCL

!        coral internal conditions

            TAcal (n,i,j)=2500.d0
            TAcoe (n,i,j)=2200.d0
            DICcal (n,i,j)=2100.d0
            DICcoe (n,i,j)=2100.d0
            DOcoe (n,i,j)=10.d0 !(umol/kg)
!            CH2O (n,i,j)=3.d5 !450 ug/cm2 = 15 umol cm-2 -> 5.e6 umol kg-1 (腔腸内平均濃度とした）
            CH2O (n,i,j)=15.d0 !450 ug/cm2 = 15 umol cm-2 -> 5.e6 umol kg-1 (腔腸内平均濃度とした）

#if defined NONE_CO2_EQ
            cCO2aqcal(n,i,j)= 9.d0
            cCO2aqcoe(n,i,j)=33.d0
#endif

#if defined CARBON_ISOTOPE
            R13C=R13C_fromd13C(-15.d0)
!          c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
            c13CH2O (n,i,j)=R13C*CH2O(n,i,j)

            R13C=R13C_fromd13C(0.d0)
!          DI13Ccoe (n,i,j)=R13C/(1.+R13C)*DICcoe (n,i,j)
            DI13Ccoe (n,i,j)=R13C*DICcoe (n,i,j)

            R13C=R13C_fromd13C(0.d0)
!          DI13Ccal (n,i,j)=R13C/(1.+R13C)*DICcal (n,i,j)
            DI13Ccal (n,i,j)=R13C*DICcal (n,i,j)

# if defined NONE_CO2_EQ
            R13C=R13C_fromd13C(-8.d0)
            c13CO2aqcal(n,i,j)=R13C*cCO2aqcal(n,i,j)

            R13C=R13C_fromd13C(-8.d0)
            c13CO2aqcoe(n,i,j)=R13C*cCO2aqcoe(n,i,j)
# endif
#endif
          enddo
        enddo
      enddo

      return
    end subroutine initialize_coral
      

!!! **********************************************************************
!!!  Main program of coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

    subroutine coral_polyp      &
!          input parameters
     &            (n,i,j          &   ! n: coral compartment; i,j: position
     &            ,dt             &   ! Time step (sec)
     &            ,PFD            &   ! Photon flux density (umol m-2 s-1)
     &            ,Tamb           &   ! Temperature (oC)
     &            ,Samb           &   ! Salinity (PSU)
     &            ,DICamb         &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TAamb          &   ! Total alkalinity (TA: umol kg-1)
     &            ,DOamb          &   ! Dissolved oxygen (umol L-1)
#if defined NUTRIENTS         
     &            ,NO3amb         &   ! NO3 (umol L-1)
     &            ,NO2amb         &   ! NO2 (umol L-1)
     &            ,NH4amb         &   ! NH4 (umol L-1)
     &            ,PO4amb         &   ! PO4 (umol L-1)
     &            ,DOCamb         &   ! Dissolved organic carbon (DOC: umol L-1)
     &            ,DONamb         &   ! Dissolved organic nitrogen (DON: umol L-1)
     &            ,DOPamb         &   ! Dissolved organic phosporius (DOP: umol L-1)
     &            ,PHYamb         &   ! phytoplankton (umol C L-1)
     &            ,ZOOamb         &   ! zooplankton (umol C L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13Camb       &   !13C of DIC (umol kg-1)
#endif
     &            ,tau_amb        &   ! bottom shear stress (N m-2)
     &            ,Fsed           &   ! sedimentation rate (??)
!          output parameters
     &            ,Pg             &   ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,R              &   ! Respiration rate (nmol cm-2 s-1)
     &            ,Gn             &   ! Galcification rate (nmol cm-2 s-1)
     &            ,DICuptake      &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,TAuptake       &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake       &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#if defined NUTRIENTS
     &            ,NO3uptake      &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NO2uptake      &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,DOCuptake      &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,DONuptake      &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,DOPuptake      &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &            ,PHYuptake      &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &            ,ZOOuptake      &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13Cuptake    &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
     &             )

!-----------------------------------------------------------------------

      use mod_geochem
      
      implicit none

! input parameters
      integer, intent(in) :: n,i,j
      real(8), intent(in) :: dt
      real(8), intent(in) :: PFD      
      real(8), intent(in) :: Tamb     
      real(8), intent(in) :: Samb     
      real(8), intent(in) :: DICamb   
      real(8), intent(in) :: TAamb    
      real(8), intent(in) :: DOamb    
#if defined NUTRIENTS
      real(8), intent(in) :: NO3amb   
      real(8), intent(in) :: NO2amb   
      real(8), intent(in) :: NH4amb   
      real(8), intent(in) :: PO4amb   
      real(8), intent(in) :: DOCamb   
      real(8), intent(in) :: DONamb   
      real(8), intent(in) :: DOPamb   
      real(8), intent(in) :: PHYamb   
      real(8), intent(in) :: ZOOamb   
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(in) :: DI13Camb 
#endif
      real(8), intent(in) :: tau_amb  
      real(8), intent(in) :: Fsed
! output parameters
      real(8), intent(out) :: Pg       
      real(8), intent(out) :: R        
      real(8), intent(out) :: Gn       
      real(8), intent(out) :: DICuptake
      real(8), intent(out) :: TAuptake 
      real(8), intent(out) :: DOuptake 
#if defined NUTRIENTS
      real(8), intent(out) :: NO3uptake
      real(8), intent(out) :: NO2uptake
      real(8), intent(out) :: NH4uptake
      real(8), intent(out) :: PO4uptake
      real(8), intent(out) :: DOCuptake
      real(8), intent(out) :: DONuptake
      real(8), intent(out) :: DOPuptake
      real(8), intent(out) :: PHYuptake
      real(8), intent(out) :: ZOOuptake
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(out) :: DI13Cuptake
#endif

!!!------------Set parameters  ----------------------------------

!----- Physical constants ------------------------
      real(8), parameter :: Rgas=8.314d0 ! Gas constant (J mol-1)

!------------Coral parameters ---------------------------------

!----- Coral basic parameters ----------------------------------------

      real(8), parameter :: coe_tick=0.3d0    !Tickness of coral tissue (cm) 3000 um Reviewer's suggestion: 1000-5000 um
      real(8), parameter :: cal_tick=0.0005d0 !Tickness of coral calcifying fluid (cm) 5um Reviewer's suggestion: 5 um

!----- for gross photosynthesi -------------------

!  Kuhl et al. 1995, MEPS 
!     Pg=Pgmax*tanh(I/Ik) for Acropora sp.

!      Pg=Pgmax(n)*tanh(PFDsurf/Ik(n))*(1-Br(i,j))
!     &       *cHCO3coe/(K_HCO3+cHCO3coe)
!     &       *coe_tick

      real(8), parameter :: Pgmax(nCL)=  0.73d0 !0.73d0*    !0.91d0    !Maximum photosynthesis rate (nmol O2 cm-2 s-1) Kuhl et al. (1995) *Tuned
      real(8), parameter :: Ik(nCL)=     275.d0    !(275 uEin m-2 s-1) * refference value

!------ Goreau et al. (1996)------------------------------------------   
!                    Coral    FIZ    CZ
!          K_HCO3   408.    71.     178.   uM
!          V_HCO3     1.45   1.09    13.   nmol O2 (ug Chl a)-1 min-1
!          K_CO2aq    5.     0.67     2.7  uM
!          V_CO2aq    2.     1.15    12.5  nmol O2 (ug Chl a)-1 min-1
!---------------------------------------------------------------------
      real(8), parameter :: K_HCO3(nCL)= 408.d0  !uM
      real(8), parameter :: V_HCO3(nCL)=  1.45d0 !nmol O2 (ug Chl a)-1 min-1
      real(8), parameter :: K_CO2aq(nCL)= 5.d0   !uM
      real(8), parameter :: V_CO2aq(nCL)= 2.d0   !nmol O2 (ug Chl a)-1 min-1

!     Temperature dependency parameters (Hikosaka et al. 2006) 
!        - Parameters estimated from data by Al-Horani (2005)

      real(8), parameter :: Hact(nCL) = 1.855d5 !Activation energy (J mol-1)) *tuned
      real(8), parameter :: Hdeact(nCL)=3.055d5 !Deactivation energy (J mol-1) *tuned
      real(8), parameter :: Entr(nCL)=  1028.d0  !Entropy term (J K-1) *tuned

!----- for respiration --------------------------

!      R= k_R(n)*CH2O(n,i,j)/(K_CH2O(n)+CH2O(n,i,j))
!     &            *DOcoe(n,i,j)/(K_DO(n)+DOcoe(n,i,j))

      real(8), parameter :: Rmax(nCL)   = 0.53d0 !0.53*/0.37/ !Maximum respiration rate (nmol O2 cm-2 s-1) *Tuned using Kuhl et al (1995), Al-Horani et al. (2003) data
      real(8), parameter :: K_CH2O(nCL) = 5.d0   !umol cm-2!!!*適当
      real(8), parameter :: K_DO(nCL)   = 46.d0  ! ca. 46 (umol kg-1) Newton & Atkinson (1991)

      real(8), parameter :: ratio(nCL)  = 1.0d0 !0.8d0  !1.0d0* !!!*適当

!     Temperature dependency parameters (Hikosaka et al. 2006) 
!        - Parameters estimated from data by Fujimura et al. (2008)

      real(8), parameter :: Eact(nCL) =  5.117d4 !Activation energy (J mol-1)) *Tuned

!----- for calcification ------------------------

!      real(8), parameter :: k_Gn(nCL) /1.d-2/!(未使用）

      real(8), parameter :: eeff = 0.3d0 !0.05d0 !0.3d0*!Energy efficency of calcification
      real(8), parameter :: E_other = 5.0d3 !1.d4 ! 5.0d3* Energy flux of other metabolisms (nJ cm-2 s-1)

      real(8), parameter :: k_CO2i = 1.5d-3 !1.5d-3* !permeability coefficient (cm s-1): Sueltemeyer and Rinast (1996): (1.49d-3 cm s-1)
      real(8), parameter :: k_TA   = 3.0d-3 !5.0d-5 !3.0d-3* !conductivity of TA through the leak pass (cm s-1) *Tuned
      real(8), parameter :: k_DIC  = 3.0d-3 !5.0d-5 !3.0d-3* !conductivity of DIC through the leak pass (cm s-1) *Tuned

!----- for carbon isotope ------------------------

#if defined CARBON_ISOTOPE
      real(8), parameter :: a_phot = -20.d-3+1.d0!!!!!!!!!!!!!!!!!!適当
      real(8), parameter :: a_resp =   0.d-3+1.d0
      real(8), parameter :: a_calc =   2.7d-3+1.d0 
      real(8), parameter :: a_co2  =  -8.d-3+1.d0!!!!!!!!!!!!!!!!!!適当-8-4.4
#endif

!--- Other variables ----------------------------------------------
      real(8) Pn
      real(8) Si   !Mass transfer coefficient (cm s-1)
      real(8) TKamb   !Temperature (K)
!----- for CO2 system dynamics -------------------
      real(8) pHcal,fCO2cal,cHCO3cal,cCO3cal,Wargcal
      real(8) pHcoe,fCO2coe,cHCO3coe,cCO3coe
      real(8) pHamb,fCO2amb,cCO2aqamb,cHCO3amb,cCO3amb,Wargamb  !!!!!不要：アウトプット用
#if defined NONE_CO2_EQ
      real(8) cHcoe,cOHcoe
      real(8) cHcal,cOHcal
      real(8) leak_CO2aq, CO2aquptake
      real(8) kp1,km1,kp4,km4
#endif
!----- for gross photosynthesis -------------------
      real(8) PFDsurf
!       Temperature related factor
      real(8) f_temp
!----- for calcification ------------------------
      real(8) E_tot,E_ca,F_H,F_CO2
      real(8) leak_TA,leak_DIC
!      real(8) leak_CO2aq,leak_HCO3,leak_CO3

!----- for nutrients dynamics -------------------

!----- for cabon isotope calculation -------------------

      real(8) d13C_DICcoe,d13C_DICcal,d13C_CH2O,d13C_DICamb,d13C_arg
#if defined CARBON_ISOTOPE
      real(8) R13Ccoe,R13Ccal,R13CH2O,R13Camb,R13Carg
      real(8) R13Cleak,R13Cfco2,R13Cuptake
      real(8) F_13CO2,k_13CO2i
      real(8) leak_DI13C
      real(8) cH13CO3coe, c13CO3coe
      real(8) cH13CO3cal, c13CO3cal
      real(8) leak_13CO2aq, leak_H13CO3, leak_13CO3
      real(8) c13CO2aquptake
      real(8) R13C_HCO3cal
      real(8) c13CO2aqamb
#endif

!  Output
      real(8), save :: time        = 0.d0  !sec
      real(8), save :: S_PFD_dt    = 0.d0
      real(8), save :: S_Gn_dt     = 0.d0
      real(8), save :: S_d13CargxGn_dt = 0.d0
      real(8), save :: S_d13CCH2O_dt= 0.d0
      real(8), save :: S_Pg_dt      = 0.d0
      real(8), save :: S_R_dt       = 0.d0
      real(8), save :: S_CH2O_dt    = 0.d0
      real(8), save :: dsec         = 0.d0 !sec
      real(8), save :: dday         = 1.d0 !day


!----------- Temperature (K)  --------------------------------

      TKamb=Tamb+273.15d0 !(oC->K)

!----------- calculate mass transfer velocity (cm s-1) -------

      Si = (65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
      
!      Si_DIC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!!      Si(nDIC)=65.7*tau_amb**0.4 * 1.e-4 + 4.1e-4!1.1e-4
!
!      Si_TA =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!!      Si(nTA )=65.7*tau_amb**0.4 *1.e-4 + 9.1e-4!1.1e-4
!
!      Si_DO =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!!      Si(nDO )=65.7*tau_amb**0.4 *1.e-4 + 2.2e-4
!
!      Si_DOC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!
!      Si_NO3=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!      Si_NH4=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!      Si_PO4=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!
!      Si_PHY=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!      Si_nZOO=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4

#if defined NUTRIENTS

!------- Nutrients flux (nmol cm-2 s-1) ------------------------

! NH4 uptake
! M Atkinson, J Falter… - Coral Reefs, 2001 
!     SiNH4 =7.5 m day-1 = 7.5*100./24./60./60. cm s-1
!     cNH4 umol kg-1 -> 1.023 nmol cm-3
!     Flux nmol cm-2 s-1

!      NH4uptake=7.5*100./86400.*cNH4*1.023
! NO3 uptake
!      NO3uptake=4.4*100./86400.*cNO3*1.023
! PO4 uptake
!      PO4uptake=7.4*100./86400.*cPO4*1.023


      NH4uptake=Si*NH4amb*1.023d0
! NO3 uptake
      NO3uptake=Si*NO3amb*1.023d0
! PO4 uptake
      PO4uptake=Si*PO4amb*1.023d0

! Coral carbon cell quoto

!      c_SQC=min((NH4uptake+NO3uptake)*c_CNP(nC)/c_CNP(nN)
!     &             ,PO4uptake*c_CNP(nC)/c_CNP(nP))
!
!      c_SQC=CH2O(n,i,j)/(1000.+CH2O(n,i,j))*c_SQC
!      c_SQN=c_SQC*c_CNP(nN)/c_CNP(nC)
!      c_SQP=c_SQC*c_CNP(nP)/c_CNP(nC)
!
!      tempb=NH4uptake/(NH4uptake+NO3uptake)*c_SQN
!      NO3uptake=NO3uptake/(NH4uptake+NO3uptake)*c_SQN
!      NH4uptake=tempb
!      PO4uptake=c_SQP

!      NH4uptake=0.
!! NO3 uptake
!      NO3uptake=0.
!! PO4 uptake
!      PO4uptake=0.
#endif

!----CO2*+H2O <-> H+ +HCO3- is none equilibrium
#if defined NONE_CO2_EQ
!----------- CO2 system in calcified fulid -------------------
      pHcal     = pH_fromATCTcCO2aq(TAcal(n,i,j),DICcal(n,i,j),cCO2aqcal(n,i,j),TKamb, Samb)
      cHCO3cal  = cHCO3_noneq(DICcal(n,i,j),cCO2aqcal(n,i,j),pHcal,TKamb, Samb)!! for Isotope
      cCO3cal   = cCO3_noneq(DICcal(n,i,j),cCO2aqcal(n,i,j),pHcal,TKamb, Samb)
      fCO2cal   = fCO2_fromcCO2aq(cCO2aqcal(n,i,j),TKamb, Samb)  !! for output
      Wargcal   = Warg_fromcCO3cCa(cCO3cal,10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed

!----------- CO2 system in coelenteron -------------------
      pHcoe     = pH_fromATCTcCO2aq(TAcoe(n,i,j),DICcoe(n,i,j),cCO2aqcoe(n,i,j),TKamb, Samb)
      cHCO3coe  = cHCO3_noneq(DICcoe(n,i,j),cCO2aqcoe(n,i,j),pHcoe,TKamb, Samb)
      cCO3coe   = cCO3_noneq(DICcoe(n,i,j),cCO2aqcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope
      fCO2coe   = fCO2_fromcCO2aq(cCO2aqcoe(n,i,j),TKamb, Samb)  !! for output

!----------- CO2 system in ambient seawater ------------------- !! for output
      pHamb   = pH_fromATCT(TAamb,DICamb,TKamb, Samb)
      cCO2aqamb = cCO2aq_fromCTpH(DICamb,pHamb,TKamb, Samb)
      cHCO3amb = cHCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
      cCO3amb = cCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
      fCO2amb   = fCO2_fromcCO2aq(cCO2aqamb,TKamb, Samb)  !! for output
      Wargamb = Warg_fromcCO3(cCO3amb,TKamb, Samb)

!----CO2 equilibrium
#else
!----------- CO2 system in calcified fulid -------------------
      pHcal     = pH_fromATCT(TAcal(n,i,j),DICcal(n,i,j),TKamb, Samb)
      cCO2aqcal(n,i,j) = cCO2aq_fromCTpH(DICcal(n,i,j),pHcal,TKamb, Samb)
      cHCO3cal  = cHCO3_fromCTpH(DICcal(n,i,j),pHcal,TKamb, Samb)!! for Isotope
      cCO3cal   = cCO3_fromCTpH(DICcal(n,i,j),pHcal,TKamb, Samb)
      fCO2cal   = fCO2_fromcCO2aq(cCO2aqcal(n,i,j),TKamb, Samb)  !! for output
      Wargcal   = Warg_fromcCO3cCa(cCO3cal,10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed

!----------- CO2 system in coelenteron -------------------
      pHcoe     = pH_fromATCT(TAcoe(n,i,j),DICcoe(n,i,j),TKamb, Samb)
      cCO2aqcoe(n,i,j) = cCO2aq_fromCTpH(DICcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope and output
      cHCO3coe  = cHCO3_fromCTpH(DICcoe(n,i,j),pHcoe,TKamb, Samb)
      cCO3coe   = cCO3_fromCTpH(DICcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope
      fCO2coe   = fCO2_fromcCO2aq(cCO2aqcoe(n,i,j),TKamb, Samb)  !! for output

# if defined TESTMODE
!----------- CO2 system in ambient seawater ------------------- !! for output
      pHamb   = pH_fromATCT(TAamb,DICamb,TKamb, Samb)
      cCO2aqamb = cCO2aq_fromCTpH(DICamb,pHamb,TKamb, Samb)
      cHCO3amb = cHCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
      cCO3amb = cCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
      fCO2amb   = fCO2_fromcCO2aq(cCO2aqamb,TKamb, Samb)  !! for output
      Wargamb = Warg_fromcCO3(cCO3amb,TKamb, Samb)
# endif
#endif  /*end NONE_CO2_EQ*/


!----- Gross photosynthesis rate (nmolO2 cm-2 s-1) -----------------

!   Sediment dependent term
      PFDsurf=PFD!*exp(-lamb*dsed)

!   Temperature dependent term
      f_temp=1.d0
!      f_temp=exp(Hact(n)*(TKamb-298.)/298./Rgas/TKamb)        &
!     &       *(1.+exp((298.*Entr(n)-Hdeact(n))/298./Rgas))    &
!     &       /(1.+exp((TKamb*Entr(n)-Hdeact(n))/TKamb/Rgas))

!   Gross photosynthesis rate (nmolO2 cm-2 s-1)
!      Pg=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))                  &
!     &     *1./(V_HCO3(n)+V_CO2aq(n))                         &
!     &       *( V_HCO3(n)*cHCO3coe/(K_HCO3(n)+cHCO3coe)       &
!     &         +V_CO2aq(n)*cCO2aqcoe(n,i,j)/(K_CO2aq(n)+cCO2aqcoe(n,i,j))   &
!     &        )

      Pg=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))                  &
     &     * cHCO3coe/(K_HCO3(n)+cHCO3coe)

!----- Respilation rate (nmolO2 cm-2 s-1) ---------------------------

!   Temperature dependent term
      f_temp=1.d0
!      f_temp=exp(Eact(n)*(TKamb-298.)/298./Rgas/TKamb)

!   Respilation rate (nmolO2 cm-2 s-1) 
      R= Rmax(n)*f_temp*CH2O(n,i,j)/(K_CH2O(n)+CH2O(n,i,j))  &
     &            *DOcoe(n,i,j)/(K_DO(n)+DOcoe(n,i,j))

!----- Calcification rate (nmol cm-2 s-1) ------------------------------------

!      Gn=k_Gn(n)*(Wargcal-1.)
      
      if (Wargcal .ge. 1.d0) then
        Gn=1.1d-3*(Wargcal-1.d0)**1.63d0
      else
        Gn=-2.7d-2*(1.d0-Wargcal)**2.5d0
      endif

!----- H+ flux through Ca2+ ATPase  (nmol cm-2 s-1) --------------------------

!   energy Flux (nJ cm-2 s-1) partition

!  C6H12O6 + 6 O2 + 38 ADP + 38 Pi -> 6 CO2 + 6 H2O + 38 ATP  dG=-2873.4 kJ/mol
!  ATP + H2O -> ADP + Pi  dG=-7.3kcal/mol=-30.5kJ/mol

      E_tot=30.5d3*38.d0/6.d0*R  !nJ cm-2 s-1
      E_ca=E_tot-E_other
      E_ca=max(E_ca,0.d0) !Error handring

!   H+ flux (nmol cm-2 s-1)

      F_H= E_ca * eeff/                                         &
     &  (2.3d0*8.31d0*(Tamb+273.15d0)                           &
     &                 *max((pHcal-pHcoe)+0.01d0, 0.01d0))
!     &          *10.**(-pHcal)*10.**8
!      F_H=max(F_H,0.) !Error handring
!      F_H=E_ca/50000.*2.

#if defined NUTRIENTS
!
!----- Plankton predation --------------------------
!      PHYuptake=Si(nPHY)*cPHY*1.023d0
      PHYuptake=0.d0
      ZOOuptake=Si*ZOOamb*1.023d0
#endif
!---------- Other fluxes (nmol cm-2 s-1) ----------------------------

!   CO2* flux (nmol cm-2 s-1)
!      F_CO2=k_CO2i*(fCO2coe-fCO2cal)
      F_CO2=k_CO2i*(cCO2aqcoe(n,i,j)-cCO2aqcal(n,i,j))*1.023d0
      
!   Leak flux (nmol cm-2 s-1) 
      leak_TA =k_TA *(TAcoe(n,i,j)-TAcal(n,i,j))*1.023d0
      leak_DIC=k_DIC*(DICcoe(n,i,j)-DICcal(n,i,j))*1.023d0
      
!      leak_TA =k_TA *(TAamb-TAcal(n,i,j))*1.023
!      leak_DIC=k_DIC*(DICamb-DICcal(n,i,j))*1.023

!   DIC and TA uptake (nmol cm-2 s-1)
      DICuptake=Si*(DICamb-DICcoe(n,i,j))*1.023d0
      TAuptake =Si*(TAamb-TAcoe(n,i,j))*1.023d0

!   DO uptake (nmol cm-2 s-1)
      DOuptake =Si*(DOamb-DOcoe(n,i,j))*1.023d0

#if defined NONE_CO2_EQ
      leak_CO2aq=k_DIC*(cCO2aqcoe(n,i,j)-cCO2aqcal(n,i,j))*1.023d0
      CO2aquptake=Si*(cCO2aqamb-cCO2aqcoe(n,i,j))*1.023d0
#endif

!!! ----- Isotope calculation ----------------------------------------------------
#if defined CARBON_ISOTOPE  

# if defined NONE_CO2_EQ
!----------- CO2 system in calcified fulid -------------------

!      cH13CO3cal  = cH13CO3_noneq(DI13Ccal(n,i,j), c13CO2aqcal(n,i,j), pHcal, TKamb, Samb)
!      c13CO3cal   = c13CO3_noneq(DI13Ccal(n,i,j), c13CO2aqcal(n,i,j), pHcal, TKamb, Samb)
      cH13CO3cal  = cH13CO3_noneq2(DI13Ccal(n,i,j), c13CO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
      c13CO3cal   = c13CO3_noneq2(DI13Ccal(n,i,j), c13CO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)

!----------- CO2 system in coelenteron -------------------

!      cH13CO3coe  = cH13CO3_noneq(DI13Ccoe(n,i,j), c13CO2aqcoe(n,i,j), pHcoe, TKamb, Samb)
!      c13CO3coe   = c13CO3_noneq(DI13Ccoe(n,i,j), c13CO2aqcoe(n,i,j), pHcoe, TKamb, Samb)
      cH13CO3coe  = cH13CO3_noneq2(DI13Ccoe(n,i,j), c13CO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
      c13CO3coe   = c13CO3_noneq2(DI13Ccoe(n,i,j), c13CO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
      
!----------- CO2 system in ambient seawater -------------------

      c13CO2aqamb = conc13CO2aq(DI13Camb,cCO2aqamb,cHCO3amb,cCO3amb,TKamb)

# else
!----------- CO2 system in calcified fulid -------------------

      c13CO2aqcal(n,i,j) = conc13CO2aq(DI13Ccal(n,i,j),cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
      cH13CO3cal  = concH13CO3(DI13Ccal(n,i,j),cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
      c13CO3cal   = conc13CO3(DI13Ccal(n,i,j),cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)

!----------- CO2 system in coelenteron -------------------

      c13CO2aqcoe(n,i,j) = conc13CO2aq(DI13Ccoe(n,i,j),cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
      cH13CO3coe  = concH13CO3(DI13Ccoe(n,i,j),cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
      c13CO3coe   = conc13CO3(DI13Ccoe(n,i,j),cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)

# endif

! Carbon isotope ratio R13C=13C/12C

!      R13CH2O=c13CH2O(n,i,j)/(CH2O(n,i,j)-c13CH2O(n,i,j))   !coral organism
!      R13Ccoe=DI13Ccoe(n,i,j)/(DICcoe(n,i,j)-DI13Ccoe(n,i,j))   !DIC in coelenteron
!      R13Ccal=DI13Ccal(n,i,j)/(DICcal(n,i,j)-DI13Ccal(n,i,j))   !DIC in calcifying fluid
!      R13Camb=DI13Camb/(DICamb-DI13Camb)!!!!!  DIC in ambient seawater
!      
!      c13CO2aqcoe=cCO2aqcoe(n,i,j)*R13Ccoe*a_co2/(1.+R13Ccoe*a_co2)
!      c13CO2aqcal(n,i,j)=cCO2aqcal(n,i,j)*R13Ccal*a_co2/(1.+R13Ccal*a_co2)


      R13CH2O=c13CH2O(n,i,j)/CH2O(n,i,j)   !coral organism
      R13Ccoe=DI13Ccoe(n,i,j)/DICcoe(n,i,j)   !DIC in coelenteron
      R13Ccal=DI13Ccal(n,i,j)/DICcal(n,i,j)   !DIC in calcifying fluid
      R13Camb=DI13Camb/DICamb!!!!!  DIC in ambient seawater
      
      R13C_HCO3cal=cH13CO3cal/cHCO3cal
      
! Carbon isotope fluxes
      F_13CO2=k_CO2i/1.0007d0*(c13CO2aqcoe(n,i,j)-c13CO2aqcal(n,i,j))*1.023d0
      DI13Cuptake=Si *(DI13Camb-DI13Ccoe(n,i,j))*1.023d0
      leak_DI13C=k_DIC*(DI13Ccoe(n,i,j)-DI13Ccal(n,i,j))*1.023d0
                    
# if defined NONE_CO2_EQ
! Carbon isotope fluxes
      c13CO2aquptake=Si *(c13CO2aqamb-c13CO2aqcoe(n,i,j))*1.023d0
      leak_13CO2aq=k_DIC*(c13CO2aqcoe(n,i,j)-c13CO2aqcal(n,i,j))*1.023d0
# endif

#endif /* End isotope calculation */

!------------ internal condition change -----------------------------

!     CH2O (umol cm-2)
!     Pg,R: nmol cm-2 s-1
      CH2O (n,i,j)=CH2O (n,i,j)                   &
!     &         +(Pg-R)/coe_tick/1.023 *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
     &         +(Pg-R)*1.d-3 *dt    !nmol cm-2 =1.e-3 umol cm-2
      CH2O(n,i,j)=max(CH2O(n,i,j),0.d0) !Error handring

!    DIC & TA (umol kg-1) in calcified fulid
      DICcal (n,i,j)=DICcal (n,i,j)              &
     &            +(-Gn                          &
     &              +R*(1.-ratio(n))             & !!!!Test: add R*(1.-ratio(n))
     &              +F_CO2                       &
     &              +leak_DIC                    &
     &             )/cal_tick/1.023d0 *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      DICcal(n,i,j)=max(DICcal(n,i,j),0.d0) !Error handring
      DICcal(n,i,j)=min(DICcal(n,i,j),100000.d0) !Error handring

      TAcal (n,i,j)=TAcal (n,i,j)               &
     &     +(-2.d0*Gn                           &
     &       +F_H                               &
     &       +leak_TA                           &
     &      )/cal_tick/1.023d0 *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      TAcal(n,i,j)=max(TAcal(n,i,j),0.d0) !Error handring
      TAcal(n,i,j)=min(TAcal(n,i,j),100000.d0) !Error handring


!    DIC & TA (umol kg-1) in coelectron
      DICcoe (n,i,j)=DICcoe (n,i,j)            &
     &            +(-Pg                        &
     &              +R*ratio(n)                & !!!!Test: add  *ratio(n)
     &              +DICuptake                 &
     &              -F_CO2                     &
     &              -leak_DIC                  &
     &             )/coe_tick/1.023d0 *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      DICcoe(n,i,j)=max(DICcoe(n,i,j),0.d0) !Error handring
      DICcoe(n,i,j)=min(DICcoe(n,i,j),100000.d0) !Error handring

      TAcoe (n,i,j)=TAcoe (n,i,j)             &
     &     +(-F_H                             &
     &        +TAuptake                       &
     &        -leak_TA                        &
     &      )/coe_tick/1.023d0 *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      TAcoe(n,i,j)=max(TAcoe(n,i,j),0.d0) !Error handring
      TAcoe(n,i,j)=min(TAcoe(n,i,j),100000.d0) !Error handring


!    DO (umol kg-1) in coelectron
      DOcoe (n,i,j)=DOcoe (n,i,j)             &
     &            +(Pg-R                      &
     &              +DOuptake                 &
     &             )/coe_tick/1.023d0 *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      DOcoe (n,i,j)=max( DOcoe (n,i,j),0.d0) !Error handring

#if defined NONE_CO2_EQ

      kp1=kp1_Johnson1982(TKamb, Samb)
      km1=km1_Johnson1982(TKamb, Samb)
      kp4=kp4_Johnson1982(TKamb, Samb)
      km4=km4_Johnson1982(TKamb, Samb)

      cHcoe =10.d0**(-pHcoe)
      cOHcoe=Kw_Millero1995(TKamb, Samb)/cHcoe
      
      cHcal =10.d0**(-pHcal)
      cOHcal=Kw_Millero1995(TKamb, Samb)/cHcal

!    [CO2*] (umol kg-1) in coelectron
      cCO2aqcoe(n,i,j)=cCO2aqcoe (n,i,j)                &
     &            +( R *ratio(n)                        & !!!!Test: add  *ratio(n)
     &              +CO2aquptake                        &
     &              -F_CO2                              &
     &              -leak_CO2aq                         &
     &             )/coe_tick/1.023d0 *dt               &   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!       kinetics of CO2 system
     &            +( (km1*cHcoe+km4)*cHCO3coe           & ! umol kg-1 s-1
     &              -(kp1+kp4*cOHcoe)*cCO2aqcoe(n,i,j)  & ! umol kg-1 s-1
     &             ) *dt   ! umol kg-1

!      cCO2aqcoe(n,i,j)=max(cCO2aqcoe(n,i,j),0.d0) !Error handring
!      cCO2aqcoe(n,i,j)=min(cCO2aqcoe(n,i,j),1000.d0) !Error handring

!    [CO2*] (umol kg-1) in calcified fulid
      cCO2aqcal(n,i,j)=cCO2aqcal(n,i,j)                 &
     &            +(R *(1.-ratio(n))                    & !!!!Test: add R*ratio(n)
     &              +F_CO2                              &
     &              +leak_CO2aq                         &
     &             )/cal_tick/1.023d0 *dt               &!nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!       kinetics of CO2 system
     &            +( (km1*cHcal+km4)*cHCO3cal           &! umol kg-1 s-1
     &              -(kp1+kp4*cOHcal)*cCO2aqcal(n,i,j)  &! umol kg-1 s-1
     &             ) *dt   ! umol kg-1
!      cCO2aqcal(n,i,j)=max(cCO2aqcal(n,i,j),0.d0) !Error handring
!      cCO2aqcal(n,i,j)=min(cCO2aqcal(n,i,j),1000.d0) !Error handring

!      write(60,*) cCO2aqcal(n,i,j),F_CO2,leak_CO2aq,(km1*cHcal+km4)*cHCO3cal,(kp1+kp4*cOHcal)*cCO2aqcal(n,i,j) !!!!!!!!!for Debug

#endif

!!! ----- Isotope calculation ----------------------------------------------------
#if defined CARBON_ISOTOPE

!      a_calc=exp((3.-0.140*Gn/0.011098)/1000.)    ! kinetic effect calcualtion: 1 mm y-1 
                   ! -> (10cm*1.4g/cm) /(365*24*60*60s*40g/mol)* 10**6 = 0.011098 umol cm-2 s-1

      c13CH2O (n,i,j)=c13CH2O (n,i,j)                   &
!     &         +(Pg*R13Ccoe*a_phot/(1.+R13Ccoe*a_phot)  &
!     &           -R*R13CH2O*a_resp/(1.+R13CH2O*a_resp)  &
     &         +(Pg*R13Ccoe*a_phot                      &
     &           -R*R13CH2O*a_resp                      &
!     &          )/coe_tick/1.023d0 *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
     &          )*1.e-3 *dt    !nmol cm-2 =1.e-3 umol cm-2

      DI13Ccoe (n,i,j)=DI13Ccoe (n,i,j)                     &
!     &            +(-Pg*R13Ccoe*a_phot/(1.+R13Ccoe*a_phot)  &
!     &              +R*R13CH2O*a_resp/(1.+R13CH2O*a_resp)   &
     &            +(-Pg*R13Ccoe*a_phot                      &
     &              +R*R13CH2O*a_resp *ratio(n)             & !!!!Test: add  *ratio(n)
     &              +DI13Cuptake                            &
     &              -F_13CO2                                &
     &              -leak_DI13C                             &
     &             )/coe_tick/1.023d0 *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

      DI13Ccal (n,i,j)=DI13Ccal (n,i,j)                     &
!     &            +(-Gn*R13Ccal*a_calc/(1.+R13Ccal*a_calc)  &
!     &            +(-Gn*R13Ccal*a_calc                      &
     &            +(-Gn*R13C_HCO3cal*a_calc                 &
     &              +R*R13CH2O*a_resp *(1.-ratio(n))        & !!!!Test: add R*(1.-ratio(n))
     &              +F_13CO2                                &
     &              +leak_DI13C                             &
     &             )/cal_tick/1.023d0 *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1


# if defined NONE_CO2_EQ

      kp1=kp1/1.013d0
      km1=km1/1.022d0
      kp4=kp4/1.011d0
      km4=km4/1.027d0

!      cHcoe =10.d0**(-pHcoe)
!      cOHcoe=Kw_Millero1995(TKamb, Samb)/cHcoe
!      
!      cHcal =10.d0**(-pHcal)
!      cOHcal=Kw_Millero1995(TKamb, Samb)/cHcal

!    [13CO2*] (umol kg-1) in coelectron
      c13CO2aqcoe(n,i,j)=c13CO2aqcoe (n,i,j)                &
     &            +( R *R13CH2O*a_resp*ratio(n)             &!!!!Test: add *ratio(n)
     &              +c13CO2aquptake                        &
     &              -F_13CO2                              &
     &              -leak_13CO2aq                         &
     &             )/coe_tick/1.023d0 *dt               &   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!       kinetics of CO2 system
     &            +( (km1*cHcoe+km4)*cH13CO3coe           & ! umol kg-1 s-1
     &              -(kp1+kp4*cOHcoe)*c13CO2aqcoe(n,i,j)  & ! umol kg-1 s-1
     &             ) *dt   ! umol kg-1

!      cCO2aqcoe(n,i,j)=max(cCO2aqcoe(n,i,j),0.d0) !Error handring
!      cCO2aqcoe(n,i,j)=min(cCO2aqcoe(n,i,j),1000.d0) !Error handring

!    [13CO2*] (umol kg-1) in calcified fulid
      c13CO2aqcal(n,i,j)=c13CO2aqcal(n,i,j)                 &
     &            +( R *R13CH2O*a_resp*(1.-ratio(n))      &!!!!Test: add R*(1.-ratio(n))
     &              +F_13CO2                              &
     &              +leak_13CO2aq                         &
     &             )/cal_tick/1.023d0 *dt               &!nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!       kinetics of CO2 system
     &            +( (km1*cHcal+km4)*cH13CO3cal           &! umol kg-1 s-1
     &              -(kp1+kp4*cOHcal)*c13CO2aqcal(n,i,j)  &! umol kg-1 s-1
     &             ) *dt   ! umol kg-1
!      cCO2aqcal(n,i,j)=max(cCO2aqcal(n,i,j),0.d0) !Error handring
!      cCO2aqcal(n,i,j)=min(cCO2aqcal(n,i,j),1000.d0) !Error handring

!      write(60,*) cCO2aqcal(n,i,j),F_CO2,leak_CO2aq,(km1*cHcal+km4)*cHCO3cal,(kp1+kp4*cOHcal)*cCO2aqcal(n,i,j) !!!!!!!!!for Debug

# endif

!------ Calculation of d13C -------------------------
      
      d13C_DICamb=d13C_fromR13C(R13Camb)
      d13C_DICcoe=d13C_fromR13C(R13Ccoe)
      d13C_CH2O=d13C_fromR13C(R13CH2O)
      d13C_DICcal=d13C_fromR13C(R13Ccal)

!      R13Carg=R13Ccal*a_calc
      R13Carg=R13C_HCO3cal*a_calc
      d13C_arg=d13C_fromR13C(R13Carg)

#endif  /* End isotope calculation */

!------------------------------------------------------------------------
! Print section (for debug)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!debug
#if defined DEBUG
      if(i.eq.35 .and. j.eq.96) then
        
        Pn=Pg-R

        write(52,*) PFD
     &   ,Pg,R,Pn,Gn,E_ca,TAcal(n,i,j),TAcoe(n,i,j),TAamb
     &   ,DICcal(n,i,j),DICcoe(n,i,j),DICamb
     &   ,CH2O(n,i,j),DOcoe(n,i,j)
     &   ,pHcal,pHcoe,pHamb,Wargcal,Wargamb,fCO2cal,fCO2coe,fCO2amb
     &   ,d13C_DICamb,d13C_DICcoe,d13C_CH2O,d13C_DICcal,d13C_arg
     &   ,DOamb,d13C_arg*Gn,E_tot,Tamb,p_coral(n,i,j)
!
      endif
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!debug

#if defined TESTMODE

      time = time +dt  ! sec
      
      if(time.ge.dsec) then
        
        Pn=Pg-R

        write(52,*) time/86400.,PFD                                        &
     &   ,Pg,R,Pn,Gn,E_ca,TAcal(n,i,j),TAcoe(n,i,j),TAamb           &
     &   ,DICcal(n,i,j),DICcoe(n,i,j),DICamb                        &
     &   ,CH2O(n,i,j),DOcoe(n,i,j)                                  &
     &   ,pHcal,pHcoe,pHamb,Wargcal,Wargamb,fCO2cal,fCO2coe,fCO2amb &
     &   ,d13C_DICamb,d13C_DICcoe,d13C_CH2O,d13C_DICcal,d13C_arg    &
     &   ,DOamb,d13C_arg*Gn                                         &
     &   ,d13C_fromR13C(c13CO2aqcal(n,i,j)/cCO2aqcal(n,i,j))        &
     &   ,d13C_fromR13C(cH13CO3cal/cHCO3cal),d13C_fromR13C(c13CO3cal/cCO3cal) &
     &   ,d13C_fromR13C(c13CO2aqcoe(n,i,j)/cCO2aqcoe(n,i,j))                  &
     &   ,d13C_fromR13C(cH13CO3coe/cHCO3coe),d13C_fromR13C(c13CO3coe/cCO3coe) &
     &   ,cCO2aqcal(n,i,j),cHCO3cal,cCO3cal &
     &   ,cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe &
     &   ,c13CO2aqcal(n,i,j),cH13CO3cal,c13CO3cal &
     &   ,c13CO2aqcoe(n,i,j),cH13CO3coe,c13CO3coe
     
      dsec=dsec+10.*60.  !!!!!!!!!!!!!!! print 10 min interval 
!      dsec=dsec+60.*60.  !!!!!!!!!!!!!!! print 1 hour interval 

      endif

! Coral record calculation section

      S_PFD_dt       =S_PFD_dt+PFD*dt
      S_Gn_dt         =S_Gn_dt+Gn*dt
      S_d13CargxGn_dt=S_d13CargxGn_dt+d13C_arg*Gn*dt
      S_d13CCH2O_dt  =S_d13CCH2O_dt+d13C_CH2O*dt
      S_Pg_dt        =S_Pg_dt+Pg*dt
      S_R_dt         =S_R_dt +R *dt
      S_CH2O_dt      =S_CH2O_dt+CH2O(n,i,j)*dt

      if((time/86400.).ge.dday) then

        write(53,*) dday             &
     &   ,S_PFD_dt*1.d-6             &   !Photon flux density (mol m-2 d-1)
     &   ,S_Gn_dt*1.d-3              &   !Calcification rate (umol cm-2 d-1)
     &   ,S_Pg_dt*1.d-3              &   !Gross photosynthesis rate (umol cm-2 d-1)
     &   ,S_R_dt*1.d-3               &   !Respiration rate (umol cm-2 d-1)
     &   ,S_CH2O_dt/24./60./60.      &   ! 1 day avaraged value of CH2O
     &   ,(S_Pg_dt-S_R_dt)*1.d-3     &   !Net photosynthesis rate (umol cm-2 d-1)
     &   ,S_d13CargxGn_dt            &
     &   ,S_d13CargxGn_dt/S_Gn_dt    &  !d13C
     &   ,S_d13CCH2O_dt/24./60./60.  & ! 1 day avaraged value of d13C_CH2O
     &   ,Wargamb

        dday=dday+1.d0 !!!!!!!!!!!!!!!! print 1 day interval
        
        S_PFD_dt=0.d0
        S_Gn_dt=0.d0
        S_d13CargxGn_dt=0.d0
        S_d13CCH2O_dt=0.d0
        S_Pg_dt=0.d0
        S_R_dt=0.d0
        S_CH2O_dt=0.d0

      endif
#endif

       return
     end subroutine coral_polyp


#if defined NUTRIENTS

!!! **********************************************************************
!!!  Zooxanthellae population dynamics
!!! **********************************************************************

    subroutine zooxanthellae                     &
!          input parameters
     &            (n,i,j                           &
     &            ,dt                              &
     &            ,PFDsurf                         &
     &            ,TKamb                           &      !Temperature (K)
     &            ,cCO2aqcoe, cHCO3coe             &
!     &            ,PO4uptake                      &  ! PO4 uptake (nmol cm-2 s-1)
     &            ,Expul                           & ! Zooxanthellae release rate by host coral (individual cm-2 s-1)
     &            ,C_excre,N_excre,P_excre         & ! Organic matter excretion controled by host coral (nmol cm-2 s-1)
!          output parameters
     &            ,Pg                              & ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,Rz                              & ! Zooxanthellae respiration rate (nmol cm-2 s-1)
     &            ,NO3_trans, NO2_trans, NH4_trans & ! Nutrients transport rate from host to Zooxanthellae (nmol cm-2 s-1)
     &            ,PO4_trans                       & ! Nutrients transport rate from host to Zooxanthellae (nmol cm-2 s-1)
     &            ,C_waste,N_waste,P_waste         & ! Waste organic matter (nmol cm-2 s-1)
     &            ,F_ROS                           & ! Flux of Reactive Oxygen Species  (nmol cm-2 s-1)
     &            ,F_ONOO                          & ! Flux of peroxynitrite  (nmol cm-2 s-1)
     &             )
!-----------------------------------------------------------------------

      implicit none

!          input parameters
      integer, intent(in) :: n,i,j
      real(8), intent(in) ::                       &
     &             dt                              &
     &            ,PFDsurf                         &
     &            ,TKamb                           & !Temperature (K)
     &            ,cCO2aqcoe, cHCO3coe             &
!     &            ,PO4uptake                       & ! PO4 uptake (nmol cm-2 s-1)
     &            ,Expul                           & ! Zooxanthellae release rate by host coral
     &            ,C_excre,N_excre,P_excre           ! Organic matter excretion controled by host coral
!          output parameters
      real(8), intent(out) ::                      &
     &             Pg                              & ! Gross photosynthesis rate (nmol cm-2 s-1)
     &            ,Rz                              & ! Zooxanthellae respiration rate (nmol cm-2 s-1)
     &            ,NO3_trans, NO2_trans, NH4_trans & ! Nutrients transport rate from host to Zooxanthellae (nmol cm-2 s-1)
     &            ,PO4_trans                       & ! Nutrients transport rate from host to Zooxanthellae (nmol cm-2 s-1)
     &            ,C_waste,N_waste,P_waste         & ! Waste organic matter (nmol cm-2 s-1)
     &            ,F_ROS                           & ! Flux of Reactive Oxygen Species  (nmol cm-2 s-1)
     &            ,F_ONOO                            ! Flux of peroxynitrite  (nmol cm-2 s-1)



!------------Set Constants  ----------------------------------
!----- Physical constants ------------------------

      real(8), parameter :: Rgas = 8.314d0 ! Gas constant (J mol-1)

!----- for gross photosynthesi -------------------
!
!  Kuhl et al. 1995, MEPS 
!     Pg=Pgmax*tanh(I/Ik) for Acropora sp.
!
!      Pg=Pgmax(n)*tanh(PFDsurf/Ik(n))*(1-Br(i,j))
!     &       *cHCO3coe/(K_HCO3+cHCO3coe)
!     &       *coe_tick

      real(8), parameter :: lamb = 1000.d0 !*適当(未使用）

!      data Pgmax   /1.0/ ! 11.*coe_tick (nmolO2 cm-3 s-1); 0.47 (nmol O2 cm-2 s-1) Kuhl et al. (1995) *Tuned
      real(8), parameter :: Pgmax(nCL) = 1.d-3 !1.0 (nmol cm-2 -s)=1.0/DENSzoox=1.0e-6 (nmol cell-1 s-1) =1.0e-3 (pmol cell-1 s-1)
      real(8), parameter :: Ik(nCL)    = 275.d0    !(275 uEin m-2 s-1) * refference value

!------ Goreau et al. (1996)------------------------------------------   
!                    Coral    FIZ    CZ
!          K_HCO3   408.    71.     178.   uM
!          V_HCO3     1.45   1.09    13.   nmol O2 (ug Chl a)-1 min-1
!          K_CO2aq    5.     0.67     2.7  uM
!          V_CO2aq    2.     1.15    12.5  nmol O2 (ug Chl a)-1 min-1
!---------------------------------------------------------------------
      real(8), parameter :: K_HCO3(nCL) = 408.d0   !uM
      real(8), parameter :: V_HCO3(nCL) = 1.45d0  !nmol O2 (ug Chl a)-1 min-1
      real(8), parameter :: K_CO2aq(nCL)= 5.d0 !/5./  !uM
      real(8), parameter :: V_CO2aq(nCL)= 2.d0 !/2./  !nmol O2 (ug Chl a)-1 min-1

!     Temperature dependency parameters (Hikosaka et al. 2006) 
!        - Parameters estimated from data by Al-Horani (2005)

      real(8), parameter :: Hact(nCL)   = 1.855d5 !Activation energy (J mol-1)) *tuned
      real(8), parameter :: Hdeact(nCL) = 3.055d5 !Deactivation energy (J mol-1) *tuned
      real(8), parameter :: Entr(nCL)   = 1028.d0   !Entropy term (J K-1) *tuned

!----- Respitration constants ---------------------------

      real(8), parameter :: Rmax = 3.d-5 !/0.37/ !Maximum respiration rate (pmol cell-1 s-1) *Tuned using Kuhl et al (1995), Al-Horani et al. (2003) data
      real(8), parameter :: K_QC = 8.d0 ! (pmol cell-1) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*適当
      real(8), parameter :: K_DO = 5.d0 ! ca. 10 (umol kg-1) Shick et al. (1990) *Tuned value

!----- Nutrients related parameters -------------------

      real(8), parameter :: Vmax_NO3tra = 1.67d-6  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう 
      real(8), parameter :: Vmax_NO2tra = 1.5d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: Vmax_NH4tra = 1.d-5  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: Vmax_PO4tra = 3.d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

      real(8), parameter :: K_NO3tra = 50.d0  ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: K_NO2tra = 10.d0  ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: K_NH4tra = 300.d0 ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう      
      real(8), parameter :: K_PO4tra = 300.d0 ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう      

      real(8), parameter :: k_NH4rel = 1.d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: k_NO3rel = 1.9d-8  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: k_NO2rel = 1.d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: k_PO4rel = 1.d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

      real(8), parameter :: Vmax_NO3red = 1.3d-6  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう 
      real(8), parameter :: Vmax_NO2red = 1.3d-6  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

      real(8), parameter :: K_NO3red = 0.014d0  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう
      real(8), parameter :: K_NO2red = 0.0015d0 !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

      real(8), parameter :: Vmax_Nass = 2.d-5  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう 
      real(8), parameter :: K_Nass = 1.d-1  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

      real(8), parameter :: Vmax_Pass = 5.d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう 
      real(8), parameter :: K_Pass = 1.d-2  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!てきとう

!----- Population dynamics parameters -------------------

      real(8), parameter :: QC0zoox = 8.d0   !(pmolC/cell)  100 pg cell-1 ~ 10 pmol cell-1 = (Verde & McCloskey 1998)
      real(8), parameter :: QN0zoox = 1.1d0  !(pmolN/cell)  calculated from C:N:P = 116:16:1
      real(8), parameter :: QP0zoox = 0.07d0 !(pmolP/cell)

      real(8), parameter :: u_max  = 2.d-5 !Maximum reproduction rate (pmol cell-1 s-1)  !!!!!!!!!!!!!!!!!!適当!!!!!!!!!!!!!!
      
      real(8), parameter :: km_CNP = 1.d-8 ! (cell cm-2 s-1)  適当!!!!!!!!!!!!!!
      real(8), parameter :: am_CNP = 1.d0
      real(8), parameter :: km_ROS = 1.d-11 !                  適当!!!!!!!!!!!!!!
      real(8), parameter :: km_min = 1.d-14   ! (cell cm-2 s-1)  適当!!!!!!!!!!!!!!

!----- for gross photosynthesi -------------------
!      Temperature related factor
      real(8)  f_temp

!----- Respiration parameters -------------------
      real(8)  SQC

!----- Nutrients related parameters -------------------
      real(8) N_dissim       ! Nitrogen dissimilation rate (pmol cell-1 s-1)
      real(8) N_assim        ! Nitrogen assimilation rate (pmol cell-1 s-1)
!      real(8) P_dissim       ! P dissimilation rate (pmol cell-1 s-1)
      real(8) P_assim        ! P assimilation rate (pmol cell-1 s-1)
      real(8) NO3_reduc      ! NO3 reductance rate (pmol cell-1 s-1)
      real(8) NO2_reduc      ! NO2 reductance rate (pmol cell-1 s-1)
                                         
!----- Metabolic parameters -------------------
      real(8)  C_repro,N_repro,P_repro

!----- Population dynamics parameters -------------------
      real(8)  mortality
      real(8)  Repro, Death



!----- Zooxanthellae metabolism  ----------------------------------

!----- Gross photosynthesis rate (pmol cell-1 s-1) -----------------


!   Temperature dependent term
      f_temp=1.
!      f_temp=exp(Hact(n)*(TKamb-298.)/298./Rgas/TKamb)      &
!     &       *(1.+exp((298.*Entr(n)-Hdeact(n))/298./Rgas))  &
!     &       /(1.+exp((TKamb*Entr(n)-Hdeact(n))/TKamb/Rgas))

!   Gross photosynthesis rate (pmol cell-1 s-1)
      Pg=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))                &
     &     *1./(V_HCO3(n)+V_CO2aq(n))                       &
     &       *( V_HCO3(n)*cHCO3coe/(K_HCO3(n)+cHCO3coe)     &
     &         +V_CO2aq(n)*cCO2aqcoe/(K_CO2aq(n)+cCO2aqcoe) &
     &        )


!----- Respilation rate (pmol cell-1 s-1) ---------------------------

!   Temperature dependent term
!      f_temp=1.
!      f_temp=exp(Eact(n)*(TKamb-298.)/298./Rgas/TKamb)

!   Respilation rate (nmolO2 cm-2 s-1) 
!      R= Rmax(n)*f_temp*CH2O(n,i,j)/(K_CH2O(n)+CH2O(n,i,j)) &
!     &            *DOcoe(n,i,j)/(K_DO(n)+DOcoe(n,i,j))

      Rz= Rmax*QCzoox(n,i,j)/(K_QC+QCzoox(n,i,j)) & ! Respilation rate (pmol cell-1 s-1)
     &         *DOcoe(n,i,j)/(K_DO+DOcoe(n,i,j))

!      SQC=QCzoox(n,i,j)-QC0zoox

!      Rz= Rmax*SQC/(K_QC+SQC)
!     &         *DOcoe(n,i,j)/(K_DO+DOcoe(n,i,j))

!----- Nutrient internal dynamics -----------------------------------

!----- Nutrients transport rate (pmol cell-1 s-1) ---------------------

!  These formulations are basically followed to Flynn and Flynn (1998)

      NH4_trans =Vmax_NH4tra*NH4coral(n,i,j)/(K_NH4tra+NH4coral(n,i,j)) &
     &          -k_NH4rel * NH4zoox(n,j,i)

      NO3_trans =Vmax_NO3tra*NO3coral(n,i,j)/(K_NO3tra+NO3coral(n,i,j)) &
     &               *(1.-NH4zoox(n,i,j)/0.1)                           &
     &          -k_NO3rel * NO3zoox(n,j,i)

      NO2_trans =Vmax_NO2tra*NO2coral(n,i,j)/(K_NO2tra+NO2coral(n,i,j)) &
     &               *(1.-NH4zoox(n,i,j)/0.1)                           &
     &          -k_NO2rel * NO2zoox(n,j,i)

      PO4_trans =Vmax_PO4tra*PO4coral(n,i,j)/(K_PO4tra+PO4coral(n,i,j))

!----- Nutrients reductance rate (pmol cell-1 s-1) ---------------------

      NO3_reduc = Vmax_NO3red*NO3zoox(n,i,j)/(K_NO3red+NO3zoox(n,i,j))
      NO2_reduc = Vmax_NO2red*NO2zoox(n,i,j)/(K_NO2red+NO2zoox(n,i,j))

!----- Nitrogen assimilation rate (pmol cell-1 s-1) ---------------------

      N_assim = Vmax_Nass* NH4zoox(n,i,j)/(K_Nass+NH4zoox(n,i,j)) 


!----- P assimilation rate (pmol cell-1 s-1) ---------------------

      P_assim = Vmax_Pass* PO4zoox(n,i,j)/(K_Pass+PO4zoox(n,i,j)) 


!----- Nitrogen dissimilation rate (pmol cell-1 s-1) ---------------------

      N_dissim = Rz * QNzoox(n,i,j)/QCzoox(n,i,j)   ! Dissimilation rate (pmol cell-1 s-1)


!----- interaction between zooxanthellae and coral -----------------

!----- Reproduction rate (pmol cell-1 s-1) ----------------------------

      C_repro=u_max*min(    1.-QP0zoox/QPzoox(n,i,j)  ,   &
     &                  min(1.-QN0zoox/QNzoox(n,i,j),     &
     &                      1.-QC0zoox/QCzoox(n,i,j) ) )
     
      if(C_repro .lt. 0.) then
        C_repro =0.
      end if
      
      N_repro=C_repro*QNzoox(n,i,j)/QCzoox(n,i,j)
      
      P_repro=C_repro*QPzoox(n,i,j)/QCzoox(n,i,j)


!----- Convert cell conditions to population dynamics parameters

      Repro=C_repro/QCzoox(n,i,j)*DENSzoox(n,i,j)  ! Reproduction rate (cell cm-2 s-1)

      mortality= km_CNP                                  & ! mortality (s-1) 
     &            *exp(-am_CNP*                          &             !* 足りない栄養素に依存（要検討!!!）
     &                min(  1.-QP0zoox/QPzoox(n,i,j)  ,  &
     &                  min(1.-QN0zoox/QNzoox(n,i,j),    &
     &                      1.-QC0zoox/QCzoox(n,i,j) ) ) &
     &             )
!     &          +km_ROS * ROS(n,i,j)                                !* ROS濃度依存（要検討!!!）  今のところゼロにしている
!     &          +km_min                                    !* 定数

      Death=mortality*DENSzoox(n,i,j)              ! Death rate (cell cm-2 s-1)


!--------Time step progressing ------------------------

!---- Nutrients (pmol/cell) ------------------

      NO3zoox(n,i,j)=NO3zoox(n,i,j)  &
     &              +(               &
     &                 NO3_trans     &
     &                -NO3_reduc     &
     &               )*dt
      NO3zoox(n,i,j)=max(NO3zoox(n,i,j),0.d0) !Error handring

      NO2zoox(n,i,j)=NO2zoox(n,i,j)  &
     &              +(               &
     &                 NO2_trans     &
     &                +NO3_reduc     &
     &                -NO2_reduc     &
     &               )*dt
      NO2zoox(n,i,j)=max(NO2zoox(n,i,j),0.d0) !Error handring

      NH4zoox(n,i,j)=NH4zoox(n,i,j)  &
     &              +(               &
     &                 NH4_trans     &
     &                +NO2_reduc     &
     &                -N_assim       &
     &                +N_dissim      &
     &               )*dt
      NH4zoox(n,i,j)=max(NH4zoox(n,i,j),0.d0) !Error handring
      

      PO4zoox(n,i,j)=PO4zoox(n,i,j)  &
     &              +(               &
     &                 PO4_trans     &
     &                -P_assim       &
     &               )*dt
      PO4zoox(n,i,j)=max(PO4zoox(n,i,j),0.d0) !Error handring

!-----Cell quota (pmol/cell) -----------------!!!!!単位変換チェック!!!!!!!!!!!!!!!!!!!!!!!!!!

      QCzoox(n,i,j)=QCzoox(n,i,j)                     &
     &              +(                                &
     &                 Pg-Rz                          &
     &                -C_excre/DENSzoox(n,i,j)*1.d3   &   !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
     &                -C_repro                        &
     &               )*dt
      QCzoox(n,i,j)=max(QCzoox(n,i,j),0.d0) !Error handring
           
      QNzoox(n,i,j)=QNzoox(n,i,j)                      &
     &              +(                                 &
     &                 N_assim                         & 
     &                -N_dissim                        &
!     &                 NO3uptake/DENSzoox(n,i,j)*1.e3  &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
!     &                +NH4uptake/DENSzoox(n,i,j)*1.e3  &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
     &                -N_excre/DENSzoox(n,i,j)*1.d3    &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
     &                -N_repro                         &
     &               )*dt
      QNzoox(n,i,j)=max(QNzoox(n,i,j),0.d0) !Error handring     
      
      QPzoox(n,i,j)=QPzoox(n,i,j)                       &
     &              +(                                  &
!     &                 PO4uptake/DENSzoox(n,i,j)*1.e3   &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
     &                 P_assim                          &
     &                -P_excre/DENSzoox(n,i,j)*1.d3     &   !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
     &                -P_repro                          &
     &               )*dt
      QPzoox(n,i,j)=max(QPzoox(n,i,j),0.d0) !Error handring

!-----Zooxanthellae density (cell cm-2) -----------------

!     DENSzoox ~ 1.e6 cells cm-2  (e.g. Higuchi et al., 2012)

      DENSzoox(n,i,j)=DENSzoox(n,i,j) &
     &             +(                 &
     &                Repro           & ! Reproduction
     &               -Death           & ! Death
     &               -Expul           & ! Expulsion by host coral
     &              )*dt
      DENSzoox(n,i,j)=max(DENSzoox(n,i,j),0.d0) !Error handring

!-----Output parameters  (nmol cm-2 s-1) --------------------------------

!----- ROS flux (nmol cm-2 s-1)------------------------------------

!  Estimated from Saragosti et al. (2010) data
      
!      F_ROS=0.002382*PFDsurf+0.006891*exp(0.1395*(TKamb-273.15))   ! (fmol min-1 cell-1)
      
!      F_ROS=(0.000354*PFDsurf+0.0672)*exp(0.0643*(TKamb-273.15))   ! (fmol min-1 cell-1)


!      F_ROS=(0.000354*PFDsurf+0.0672)*exp(0.0643*(TKamb-273.15))   ! (fmol min-1 cell-1)

      F_ROS=(0.000354*PFDsurf+0.0672)*exp(0.4*(TKamb-273.15))*2.d-4   ! (fmol min-1 cell-1)

      
      F_ROS=F_ROS*DENSzoox(n,i,j)/60.*1.d-6   ! (nmol cm-2 s-1)
      
!----- peroxynitrite (ONOO-) flux (nmol cm-2 s-1)------------------------------------
!
!      F_ONOO=1.e0 * NO2zoox(n,i,j)*ROS(n,i,j)   ! (pmol cell-1 s-1)!!!!!!!!!!!!!!!!!!てきとう
!      
!      F_ONOO=F_ONOO*DENSzoox(n,i,j)*1.d-3   ! (nmol cm-2 s-1)
!


!------------------------------------------------------------------------
! Print section (for debug)

# if defined TESTMODE

!      if(iprint.eq.0) then
!
!        write(54,*) &
!     &    time,PFDsurf,Pg,Rz  &
!     &   ,DENSzoox(n,i,j),QCzoox(n,i,j),QNzoox(n,i,j),QPzoox(n,i,j) &
!     &   ,ROS(n,i,j) &
!     &   ,C_repro,N_repro,P_repro &
!     &   ,Repro,Death,Expul, F_ROS &
!     &   ,NO3zoox(n,i,j),NO2zoox(n,i,j),NH4zoox(n,i,j),F_ONOO &
!     &   ,NO3_trans,NO2_trans,NH4_trans &
!     &   ,NO3_reduc,NO2_reduc &
!     &   ,N_assim,N_dissim &
!     &   ,PO4zoox(n,i,j),PO4_trans,P_assim
!
!      endif

# endif
!-----------------------------------------------------------------------


!----- Outoput for coral internal system model (nmol cm-2 s-1)------------------------------------

      Pg=Pg*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
      Rz=Rz*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)

      NO3_trans=NO3_trans*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
      NO2_trans=NO2_trans*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
      NH4_trans=NH4_trans*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
      PO4_trans=PO4_trans*DENSzoox(n,i,j)*1.d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)

      C_waste = (Death+Expul)*QCzoox(n,i,j)*1.d-3  !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1)
      N_waste = (Death+Expul)*QNzoox(n,i,j)*1.d-3
      P_waste = (Death+Expul)*QPzoox(n,i,j)*1.d-3

!---------------------------------------------------------------------

      return
    end subroutine zooxanthellae
#endif
      
  end module mod_coral

