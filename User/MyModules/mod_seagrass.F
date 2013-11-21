
!!!=== ver 2013/01/09   Copyright (c) 2012-2013 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!*************** SEAGRASS ***********************************************


  module mod_seagrass

    implicit none

    integer, parameter :: nSG = 1    !! Number of seagrass groups
    real(8), allocatable, save :: CH2Osg(:,:,:)
#if defined CARBON_ISOTOPE
!  13C isotope
    real(8), allocatable, save :: c13CH2Osg(:,:,:)
#endif

  contains
!!! **********************************************************************
!!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

    subroutine initialize_seagrass(LBi, UBi, LBj, UBj)

      use mod_geochem
      
      implicit none
! input parameters
      integer, intent(in) :: LBi, UBi, LBj, UBj
      real(8)  R13C
      integer i,j,n


      allocate( CH2Osg(nSG,LBi:UBi,LBj:UBj)       )

#if defined CARBON_ISOTOPE
      allocate( c13CH2Osg(nSG,LBi:UBi,LBj:UBj)   )
#endif

!------------------------------------------
!  Set initial conditions
      do j=LBj,UBj
        do i=LBi,UBi
          do n=1,nSG
!        seagrass internal conditions
            CH2Osg(n,i,j)=15.d0 !450 ug/cm2 = 15 umol cm-2 -> 5.e6 umol kg-1 (çoí∞ì‡ïΩãœîZìxÇ∆ÇµÇΩÅj
#if defined CARBON_ISOTOPE
            R13C=R13C_fromd13C(-15.d0)
!          c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
            c13CH2Osg(n,i,j)=R13C*CH2Osg(n,i,j)
#endif
          enddo
        enddo
      enddo
      
      return
    end subroutine initialize_seagrass

!!! **********************************************************************
!!!  Main program of seagrass model (Watanabe et al. 2012)
!!! **********************************************************************

    subroutine seagrass           &
!          input parameters
     &            (n,i,j          &   ! n: coral compartment; i,j: position
     &            ,PFD            &   ! Photon flux density (umol m-2 s-1)
     &            ,DICamb         &   ! DIC (umol kg-1)
     &            ,DOamb          &   ! DO (umol L-1)
#if defined NUTRIENTS         
     &            ,NH4amb         &   ! NH4 concentration (umol L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13Camb       &   ! 13C of DIC (umol kg-1)
#endif
!          output parameters
     &            ,Pg             &   ! Gross photosynthesis rate (mmol m-2 s-1)
     &            ,R              &   ! Respiration rate (mmol m-2 s-1)
     &            ,DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#if defined NUTRIENTS         
     &            ,NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13Cuptake    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
     &             )

!-----------------------------------------------------------------------
      use mod_geochem
      
      implicit none

! input parameters
      integer, intent(in) :: n,i,j
      real(8), intent(in) :: PFD
      real(8), intent(in) :: DICamb
      real(8), intent(in) :: DOamb
#if defined NUTRIENTS         
      real(8), intent(in) :: NH4amb
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(in) :: DI13Camb
#endif
! output parameters
      real(8), intent(out) :: Pg
      real(8), intent(out) :: R
      real(8), intent(out) :: DICuptake
      real(8), intent(out) :: DOuptake
#if defined NUTRIENTS         
      real(8), intent(out) :: NO3uptake
      real(8), intent(out) :: NH4uptake
      real(8), intent(out) :: PO4uptake
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(out) :: DI13Cuptake
#endif

! --- C:N:P ratio of seagrass ---
      real(8), parameter :: nc=27./599.d0 !M.J.Atkinson and SV Smith(1983)
      real(8), parameter :: pc=1./599.d0
! --- Photosynthesis and Calcification Parameters (coral) ---
      real(8), parameter :: pmax =  51.3d0 !average of 2009 observation and nakamura
      real(8), parameter :: pIk  = 589.65d0
      real(8), parameter :: p0   =  15.05d0 !-15.05
#if defined NUTRIENTS         
      real(8) npref
      real(8) ldocn,ldocd
#endif
#if defined CARBON_ISOTOPE
      real(8), parameter :: a_phot  = -7.d-3+1.d0  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
      real(8), parameter :: a_resp  =  0.d-3+1.d0  !
      real(8) R13C_DIC, R13C_CH2O
#endif

! --- Organic and Inorganic Production Rate -----------------

      Pg= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
      R = p0/3600.d0   !Light response curve [mmolC/m2/s]
      
      IF(DICamb<=0.d0) THEN !-----For Error handling
        Pg =0.d0
      ENDIF
      IF(DOamb<=0.d0) THEN !-----For Error handling
        R=0.d0
      ENDIF
      
      DICuptake= Pg-R
      DOuptake = R -Pg

!!! ----- Isotope calculation ----------------------------------------------------
#if defined CARBON_ISOTOPE
      R13C_DIC  = DI13Camb/DICamb
      R13C_CH2O = c13CH2Osg(n,i,j)/CH2Osg(n,i,j)
      
      IF(DI13Camb<=0.d0) THEN !-----For Error handling
        R13C_DIC =0.d0
      ENDIF

      DI13Cuptake=Pg*R13C_DIC*a_phot - R*R13C_CH2O*a_resp
#endif
      
! --- Nutrient fluxes between water column and coral -----
#if defined NUTRIENTS         
      npref= ninter(NH4amb)

      NO3uptake=Pg*nc*npref         !Flux caliculated in Carbon assimilated by Photosynthesis times C:N:P ratio [mmol/m2/s]
      NH4uptake=Pg*nc*(1.-npref)-R*nc
      PO4uptake=(Pg-R)*pc
#endif
      return
    end subroutine seagrass

!-----------------------------------------------------------------------
#if defined NUTRIENTS

!!!*********************************************************************
!!!  Caliculate ammnomiun inhibitation on nitrate uptake
!!!*********************************************************************
         

    real(8) function ninter(NH4amb)

      implicit none
      real(8), intent(in) :: NH4amb

      ninter=0.15
      if(NH4amb.lt.0.1e0) ninter=0.9

      return
    end function ninter

#endif

!!!*********************************************************************
!c
!      subroutine seagrass
!c          input parameters
!     &            (n,i,j
!     &            ,PFD,Tbtm,Sbtm,TAbtm,DICbtm
!     &            ,pH,cCO2aq,cHCO3,cCO3,fCO2,Warg
!     &            ,cNH4,cNO3,cNO2,cPO4,cTOC,cTON,cTOP,cPP1,cZP1,cDO,Fsed
!     &            ,tau_btm
!c          output parameters
!     &            ,Pg,Pn,R,Gn
!     &            ,DICuptake,TAuptake
!     &            ,NH4uptake,NO3uptake,NO2uptake,PO4uptake
!     &            ,TOCuptake,TONuptake,TOPuptake
!     &            ,DOuptake
!     &            ,PP1uptake,ZP1uptake)
!c
!C **********************************************************************
!C *                                                                    *
!C * FUNCTION    : Seagrass Metabolism.                                 *
!C *                                                                    *
!C **********************************************************************
!C
!      implicit none
!C
!      include 'pom2k_v9.blk'
!c
!      integer n,i,j
!      double precision  PFD,Tbtm,Sbtm,TAbtm,DICbtm
!     &    ,pH,cCO2aq,cHCO3,cCO3,fCO2,Warg
!     &    ,cNH4,cNO3,cNO2,cPO4,cTOC,cTON,cTOP,cPP1,cZP1,cDO,Fsed
!     &    ,tau_btm
!     &    ,Pg,Pn,R,Gn
!     &    ,DICuptake,TAuptake
!     &    ,NH4uptake,NO3uptake,NO2uptake,PO4uptake
!     &    ,TOCuptake,TONuptake,TOPuptake
!     &    ,DOuptake
!     &    ,PP1uptake,ZP1uptake
!c
!c
!c----- for gross photosynthesi -------------------
!      double precision  PFDsurf,lamb,Pgmax(scom),Ik(scom)
!c----- for respiration --------------------------
!      double precision  Rdark(scom)
!c----- for nutrients uptake ----------------------
!      double precision  Si(ncom)!,Sc,Dmol(ncom)
!c      double precision  V_NH4(scom),V_NO3(scom),V_PO4(scom)
!c      double precision  K_NH4(scom),K_NO3(scom),K_PO4(scom)
!c      double precision  t_NH4(scom)
!c
!!------------Set Constants  ----------------------------------
!c
!c----- for gross photosynthesi -------------------
!c
!      data lamb /1000./
!      data Pgmax /15./ !umolO2 m-2 s-1
!      data Ik /663./   !uEin m-2 s-1
!c
!c----- for respiration --------------------------
!c
!      data Rdark /5.8/
!c
!c----- for calcification ------------------------
!c
!c----- for nutrients dynamics --------------------
!c
!c      data V_NH4 /1./
!c      data V_NO3 /1./
!c      data V_PO4 /0.1/
!c      data K_NH4 /0.2/
!c      data K_NO3 /2./
!c      data K_PO4 /0.2/
!c      data t_NH4 /1.5/
!c
!c---- for mucus relrase --------------------------
!c
!c----------- calculate mass transfer velocity (m s-1) -------
!c
!c      Sc=km(i,j,kbm1)/Dmol(nDIC)
!c      Si(nDIC)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nDIC)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nTA )
!c      Si(nTA )=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nTA )=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nTOC)
!c      Si(nTOC)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!c      Si(nTOC)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nNO3)
!c      Si(nNO3)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nNO3)=(65.7*tau_btm**0.4+4.7)*1.e-6
!
!      Si(nNO2)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nNH4)
!c      Si(nNH4)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nNH4)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nPO4)
!c      Si(nPO4)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nPO4)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nDO )
!c      Si(nDO )=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!      Si(nDO )=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nPP1)
!c      Si(nPP1)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!c      Si(nPP1)=(65.7*tau_btm**0.4+4.7)*1.e-6
!cc
!c      Sc=km(i,j,kbm1)/Dmol(nZP1)
!c      Si(nZP1)=2850.*(2.*tau_btm/1024.)**0.38*Sc**(-0.6)*100./86400.
!c      Si(nZP1)=(65.7*tau_btm**0.4+4.7)*1.e-6
!c
!!----------------------------------------------------------
!c
!c----- Gross photosynthesis rate (umolO2 m-2 s-1) -----------------
!c
!      PFDsurf=PFD!*exp(-lamb*dsed)
!c
!c      Pg=Pgmax(n)*tanh(PFDsurf/Ik(n))*(1-Br(i,j))
!      Pg=0.0059*PFDsurf !!!!Ç∆ÇËÇ†Ç¶Ç∏*************************************
!c
!c----- Respilation rate (umolO2 m-2 s-1) ---------------------------
!c
!      R=Rdark(n)
!c
!c----- Nutrients uptake rate (umol m-2 s-1) ----------------------
!c
!c      NH4uptake=V_NH4(n)*(cNH4/(K_NH4(n)+cNH4))
!c      NO3uptake=V_NO3(n)*(cNO3/(K_NO3(n)+cNO3)*exp(-t_NH4(n)*cNH4))
!c      PO4uptake=V_PO4(n)*(cPO4/(K_PO4(n)+cPO4))
!c
!      NH4uptake=Si(nNH4)*cNH4*1.024e2
!c NO3 uptake
!      NO3uptake=Si(nNO3)*cNO3*1.024e2
!c NO2 uptake
!      NO2uptake=Si(nNO2)*cNO2*1.024e2
!c PO4 uptake
!      PO4uptake=Si(nPO4)*cPO4*1.024e2
!c
!c
!c----- Others uptake rate (umol m-2 s-1) ----------------------
!c
!      TOCuptake=0.
!      TONuptake=0.
!      TOPuptake=0.
!      PP1uptake=0.
!      ZP1uptake=0.
!c
!c------ Total flux (umol m-2 s-1) --------------------
!c
!      Pn=(Pg-R)!*c_QC(n,i,j)!(nmol cm-2 s-1)
!c
!      DOuptake=-Pn!(nmol cm-2 s-1)
!c
!c      TAuptake =2*Gn
!      DICuptake=DICuptake*p_sgrass(n,i,j)!(umol m-2 s-1)
!      TAuptake =TAuptake *p_sgrass(n,i,j)!(umol m-2 s-1)
!      NH4uptake=NH4uptake*p_sgrass(n,i,j)
!      NO3uptake=NO3uptake*p_sgrass(n,i,j)
!      NO2uptake=NO2uptake*p_sgrass(n,i,j)
!      PO4uptake=PO4uptake*p_sgrass(n,i,j)
!      TOCuptake=TOCuptake*p_sgrass(n,i,j)
!      TONuptake=TONuptake*p_sgrass(n,i,j)
!      TOPuptake=TOPuptake*p_sgrass(n,i,j)
!      DOuptake =DOuptake *p_sgrass(n,i,j)
!      PP1uptake=PP1uptake*p_sgrass(n,i,j)
!      ZP1uptake=ZP1uptake*p_sgrass(n,i,j)
!c
!c------------------------------------------------------------------------
!cc
!       return
!      end


  end module mod_seagrass

